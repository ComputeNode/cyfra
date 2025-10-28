package io.computenode.cyfra.satellite.web

import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.headers.*
import org.http4s.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import com.comcast.ip4s.*
import scala.concurrent.duration.*
import java.nio.file.{Files as NIOFiles, Path as JPath, Paths}
import fs2.io.file.{Path as FPath, Files as FS2Files}
import java.time.LocalDate
import java.util.Base64
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.satellite.data.*
import io.computenode.cyfra.satellite.spectral.*
import io.computenode.cyfra.satellite.visualization.*

/** REST API for satellite data analysis */
object SatelliteWebServer extends IOApp:
  
  given FS2Files[IO] = FS2Files.forIO
  
  // JSON request/response models
  case class AnalysisRequest(
      tileId: String,
      date: String,
      indices: List[String] // e.g., ["NDVI", "EVI", "NDWI"]
  )
  
  case class AnalysisResponse(
      tileId: String,
      date: String,
      width: Int,
      height: Int,
      indices: Map[String, IndexResult]
  )
  
  case class IndexResult(
      name: String,
      min: Float,
      max: Float,
      mean: Float,
      stdDev: Float,
      imageUrl: String
  )
  
  case class TileInfoResponse(
      id: String,
      name: String,
      country: String,
      region: String,
      category: String,
      description: String,
      latitude: Double,
      longitude: Double,
      keywords: List[String]
  )
  
  case class ErrorResponse(error: String)
  
  case class ProductInfo(
      date: String,
      name: String,
      size_mb: Long,
      online: Boolean
  )
  
  case class AvailableDatesResponse(
      tile: String,
      products: List[ProductInfo]
  )
  
  // Output directory for results
  val outputDir: JPath = Paths.get("satellite_output")
  val dataDir: JPath = Paths.get("satellite_data")
  
  // Initialize GPU runtime (shared across requests)
  given cyfraRuntime: VkCyfraRuntime = VkCyfraRuntime()
  val analyzer = new SpectralAnalyzer()
  
  // Query parameter matchers
  object TileQueryParamMatcher extends QueryParamDecoderMatcher[String]("tile")
  object SearchQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("q")
  object CategoryQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("category")
  object RegionQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("region")
  object CountryQueryParamMatcher extends OptionalQueryParamDecoderMatcher[String]("country")
  
  /** Convert TileCatalog.TileInfo to TileInfoResponse */
  def toResponse(tile: TileCatalog.TileInfo): TileInfoResponse =
    TileInfoResponse(
      id = tile.id,
      name = tile.name,
      country = tile.country,
      region = tile.region,
      category = tile.category,
      description = tile.description,
      latitude = tile.latitude,
      longitude = tile.longitude,
      keywords = tile.keywords
    )
  
  /** Main HTTP routes */
  def routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    
    // Serve static files (frontend)
    case request @ GET -> Root / "static" / path =>
      StaticFile.fromPath(FPath(s"cyfra-satellite/static/$path"), Some(request))
        .getOrElseF(NotFound())
    
    // Main UI
    case GET -> Root =>
      StaticFile.fromPath(FPath("cyfra-satellite/static/index.html"), None)
        .getOrElseF(NotFound())
    
    // Get all tiles or search
    case GET -> Root / "api" / "tiles" :? SearchQueryParamMatcher(searchQuery) 
                                        +& CategoryQueryParamMatcher(category)
                                        +& RegionQueryParamMatcher(region)
                                        +& CountryQueryParamMatcher(country) =>
      IO {
        val filtered = (searchQuery, category, region, country) match {
          case (Some(q), _, _, _) => TileCatalog.search(q)
          case (_, Some(cat), _, _) => TileCatalog.byCategory(cat)
          case (_, _, Some(reg), _) => TileCatalog.byRegion(reg)
          case (_, _, _, Some(ctr)) => TileCatalog.byCountry(ctr)
          case _ => TileCatalog.catalog
        }
        filtered.map(toResponse)
      }.flatMap(tiles => Ok(tiles.asJson))
    
    // Get tile by ID
    case GET -> Root / "api" / "tiles" / tileId =>
      IO {
        TileCatalog.findById(tileId).map(toResponse)
      }.flatMap {
        case Some(tile) => Ok(tile.asJson)
        case None => NotFound(ErrorResponse(s"Tile $tileId not found").asJson)
      }
    
    // Get available categories
    case GET -> Root / "api" / "categories" =>
      Ok(TileCatalog.categories.asJson)
    
    // Get available regions
    case GET -> Root / "api" / "regions" =>
      Ok(TileCatalog.regions.asJson)
    
    // Get available countries
    case GET -> Root / "api" / "countries" =>
      Ok(TileCatalog.countries.asJson)
    
    // Get available dates for a tile
    case GET -> Root / "api" / "available-dates" :? TileQueryParamMatcher(tileStr) =>
      IO {
        // Get OAuth credentials
        val (username, password, clientId, clientSecret) = CopernicusAuth.getCredentialsFromEnv()
        
        if (username.isEmpty && clientId.isEmpty) {
          ErrorResponse("No Copernicus credentials configured").asJson
        } else {
          CopernicusAuth.getAccessToken(clientId, clientSecret, username, password) match {
            case scala.util.Success(token) =>
              CopernicusOData.searchProductsSimple(tileStr, token) match {
                case scala.util.Success(prods) =>
                  val productInfos = prods.map { p =>
                    ProductInfo(
                      date = p.ContentDate.Start.take(10),
                      name = p.Name,
                      size_mb = p.ContentLength / 1_000_000,
                      online = p.Online
                    )
                  }
                  AvailableDatesResponse(tileStr, productInfos).asJson
                case scala.util.Failure(e) =>
                  ErrorResponse(s"Failed to search products: ${e.getMessage}").asJson
              }
            case scala.util.Failure(e) =>
              ErrorResponse(s"Authentication failed: ${e.getMessage}").asJson
          }
        }
      }.flatMap(json => Ok(json))
    
    // Analyze a tile
    case request @ POST -> Root / "api" / "analyze" =>
      request.as[AnalysisRequest].flatMap { req =>
        IO {
          println(s"\n=== Analysis Request ===")
          println(s"Tile: ${req.tileId}, Date: ${req.date}, Indices: ${req.indices.mkString(", ")}")
          
          // Determine which bands are needed based on requested indices
          val needsNBR = req.indices.exists(_.toUpperCase == "NBR")
          val bands = if (needsNBR) {
            List("B02", "B03", "B04", "B08", "B12")  // RGB, NIR, SWIR for NBR
          } else {
            List("B02", "B03", "B04", "B08")  // RGB, NIR only
          }
          
          // Load real or synthetic satellite data
          val satelliteImage = if (req.tileId == "synthetic" || req.tileId == "demo") {
            // Synthetic mode
            println(s"  Generating synthetic data...")
            DataLoader.loadSyntheticData(512, 512)
          } else {
            // Real data mode
            val tile = RealDataLoader.TileId.parse(req.tileId).getOrElse(
              throw new IllegalArgumentException(s"Invalid tile ID: ${req.tileId}")
            )
            val date = LocalDate.parse(req.date)
            
            println(s"  Loading real Sentinel-2 data for tile $tile on $date...")
            RealDataLoader.loadSentinel2Scene(tile, date, bands, dataDir, targetSize = 1024).getOrElse {
              throw new RuntimeException(s"Failed to load scene for ${req.tileId} on ${req.date}")
            }
          }
          
          NIOFiles.createDirectories(outputDir)
          
          // Compute requested indices
          val results = req.indices.map { indexName =>
            val result = indexName.toUpperCase match {
              case "NDVI" => analyzer.computeNDVI(satelliteImage)
              case "EVI" => analyzer.computeEVI(satelliteImage)
              case "NDWI" => analyzer.computeNDWI(satelliteImage)
              case "NBR" => analyzer.computeNBR(satelliteImage)
              case other => throw new IllegalArgumentException(s"Index $other not yet implemented (available: NDVI, EVI, NDWI, NBR)")
            }
            
            // Export visualization
            val filename = s"${req.tileId}_${req.date}_${indexName}.png"
            val path = outputDir.resolve(filename)
            ImageExporter.exportWithAutoColorMap(result, path).get
            
            val stats = result.statistics
            indexName -> IndexResult(
              name = indexName,
              min = stats.min,
              max = stats.max,
              mean = stats.mean,
              stdDev = stats.stdDev,
              imageUrl = s"/api/images/$filename"
            )
          }.toMap
          
          AnalysisResponse(
            tileId = req.tileId,
            date = req.date,
            width = satelliteImage.width,
            height = satelliteImage.height,
            indices = results
          )
        }.attempt.flatMap {
          case Right(response) => Ok(response.asJson)
          case Left(error) =>
            println(s"Error: ${error.getMessage}")
            error.printStackTrace()
            InternalServerError(ErrorResponse(error.getMessage).asJson)
        }
      }
    
    // Serve generated images
    case GET -> Root / "api" / "images" / filename =>
      val path = outputDir.resolve(filename)
      if (NIOFiles.exists(path)) {
        StaticFile.fromPath(FPath.fromNioPath(path), None).getOrElseF(NotFound())
      } else {
        NotFound()
      }
    
    // Use synthetic data for testing
    case request @ POST -> Root / "api" / "analyze-synthetic" =>
      request.as[SyntheticAnalysisRequest].flatMap { req =>
        IO {
          println(s"\n=== Synthetic Analysis Request ===")
          println(s"Size: ${req.width}x${req.height}, Indices: ${req.indices.mkString(", ")}")
          
          // Generate synthetic data
          val satelliteImage = DataLoader.loadSyntheticData(req.width, req.height)
          
          NIOFiles.createDirectories(outputDir)
          
          // Compute requested indices
          val results = req.indices.map { indexName =>
            val result = indexName.toUpperCase match {
              case "NDVI" => analyzer.computeNDVI(satelliteImage)
              case "EVI" => analyzer.computeEVI(satelliteImage)
              case "NDWI" => analyzer.computeNDWI(satelliteImage)
              case "NBR" if satelliteImage.hasBand("SWIR") => analyzer.computeNBR(satelliteImage)
              case "NBR" => throw new IllegalArgumentException("NBR requires SWIR band (not available in synthetic data)")
              case other => throw new IllegalArgumentException(s"Index $other not yet implemented (available: NDVI, EVI, NDWI, NBR)")
            }
            
            // Export visualization
            val filename = s"synthetic_${System.currentTimeMillis()}_${indexName}.png"
            val path = outputDir.resolve(filename)
            ImageExporter.exportWithAutoColorMap(result, path).get
            
            val stats = result.statistics
            indexName -> IndexResult(
              name = indexName,
              min = stats.min,
              max = stats.max,
              mean = stats.mean,
              stdDev = stats.stdDev,
              imageUrl = s"/api/images/$filename"
            )
          }.toMap
          
          AnalysisResponse(
            tileId = "synthetic",
            date = LocalDate.now().toString,
            width = satelliteImage.width,
            height = satelliteImage.height,
            indices = results
          )
        }.attempt.flatMap {
          case Right(response) => Ok(response.asJson)
          case Left(error) =>
            println(s"Error: ${error.getMessage}")
            error.printStackTrace()
            InternalServerError(ErrorResponse(error.getMessage).asJson)
        }
      }
  }
  
  case class SyntheticAnalysisRequest(
      width: Int,
      height: Int,
      indices: List[String]
  )
  
  // JSON entity decoders
  given EntityDecoder[IO, AnalysisRequest] = jsonOf[IO, AnalysisRequest]
  given EntityDecoder[IO, SyntheticAnalysisRequest] = jsonOf[IO, SyntheticAnalysisRequest]
  
  def run(args: List[String]): IO[ExitCode] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(routes.orNotFound)
      .build
      .use { server =>
        IO {
          println("\n" + "=" * 80)
          println("Cyfra Satellite Analysis Web Server")
          println("=" * 80)
          println(s"Server started at http://localhost:8080")
          println(s"Open your browser to start analyzing satellite data!")
          println("=" * 80 + "\n")
        } *> IO.never
      }
      .as(ExitCode.Success)

