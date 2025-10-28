package io.computenode.cyfra.satellite.data

import io.circe.parser.*
import io.circe.generic.auto.*
import io.circe.*
import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.nio.charset.StandardCharsets
import scala.util.{Try, Success, Failure}

/** OData API client for Copernicus Data Space Ecosystem
  *
  * Documentation: https://documentation.dataspace.copernicus.eu/APIs/OData.html
  */
object CopernicusOData:

  private val ODATA_BASE = "https://catalogue.dataspace.copernicus.eu/odata/v1"
  private val httpClient = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  case class Product(
      Id: String,
      Name: String,
      ContentDate: ContentDate,
      Online: Boolean,
      ContentLength: Long
  )
  
  case class ContentDate(
      Start: String,
      End: String
  )
  
  case class ProductsResponse(
      value: List[Product]
  )

  /** Simple product search without date filtering (for debugging)
    *
    * @param tileId Tile ID (e.g. "31UCS")
    * @param accessToken OAuth token
    * @return List of most recent products
    */
  def searchProductsSimple(tileId: String, accessToken: String): Try[List[Product]] = Try {
    // Very simple query - just find recent Sentinel-2 products for this tile
    val filter = s"contains(Name,'T$tileId') and Collection/Name eq 'SENTINEL-2'"
    val orderby = "ContentDate/Start desc"
    val encodedFilter = URLEncoder.encode(filter, StandardCharsets.UTF_8)
    val encodedOrderby = URLEncoder.encode(orderby, StandardCharsets.UTF_8)
    val url = s"$ODATA_BASE/Products?$$filter=$encodedFilter&$$orderby=$encodedOrderby&$$top=10"
    
    println(s"  Simple search for tile $tileId...")
    
    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("Authorization", s"Bearer $accessToken")
      .GET()
      .build()
    
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    
    if (response.statusCode() == 200) {
      decode[ProductsResponse](response.body()) match {
        case Right(productsResp) =>
          productsResp.value
        case Left(error) =>
          throw new RuntimeException(s"Failed to parse: ${error.getMessage}")
      }
    } else {
      throw new RuntimeException(s"HTTP ${response.statusCode()}: ${response.body()}")
    }
  }

  /** Search for Sentinel-2 L2A products
    *
    * @param tile Tile identifier (e.g., "31UCS")
    * @param date Date of acquisition
    * @param accessToken OAuth access token
    * @return List of matching products
    */
  def searchProducts(
      tile: RealDataLoader.TileId,
      date: LocalDate,
      accessToken: String
  ): Try[List[Product]] = Try {
    val tileStr = tile.toString
    
    // Search for products near this date (within 7 days before/after)
    // Sentinel-2 revisit time is 5 days, so this ensures we find something
    val dateStart = date.minusDays(7).atStartOfDay().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "Z"
    val dateEnd = date.plusDays(7).atStartOfDay().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "Z"
    
    // Search for L2A products (atmospherically corrected) with the tile
    // L2A products are preferred for analysis and may have better download support
    val filter = s"contains(Name,'T$tileStr') and contains(Name,'MSIL2A') and " +
                 s"Collection/Name eq 'SENTINEL-2' and " +
                 s"ContentDate/Start ge $dateStart and " +
                 s"ContentDate/Start lt $dateEnd and " +
                 s"Online eq true"
    
    val encodedFilter = URLEncoder.encode(filter, StandardCharsets.UTF_8)
    val encodedOrderby = URLEncoder.encode("ContentDate/Start desc", StandardCharsets.UTF_8)
    val url = s"$ODATA_BASE/Products?$$filter=$encodedFilter&$$orderby=$encodedOrderby&$$top=5"
    
    println(s"  Searching Copernicus catalog for tile $tileStr on $date...")
    println(s"  Query: $url")
    
    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("Authorization", s"Bearer $accessToken")
      .GET()
      .build()
    
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    
    if (response.statusCode() == 200) {
      decode[ProductsResponse](response.body()) match {
        case Right(productsResp) =>
          if (productsResp.value.isEmpty) {
            println(s"  ⚠️  No products found for tile $tileStr on $date")
            println(s"     Try a different date or tile")
          } else {
            println(s"  ✓ Found ${productsResp.value.size} product(s)")
            productsResp.value.foreach { p =>
              println(s"     - ${p.Name} (${p.ContentLength / 1_000_000} MB, Online: ${p.Online})")
            }
          }
          productsResp.value
        case Left(error) =>
          println(s"  Response body: ${response.body().take(500)}")
          throw new RuntimeException(s"Failed to parse products: ${error.getMessage}")
      }
    } else {
      throw new RuntimeException(
        s"Product search failed: HTTP ${response.statusCode()}\n" +
        s"Response: ${response.body()}"
      )
    }
  }

  /** Get download URL for a specific band within a product
    *
    * @param productId Product UUID
    * @param band Band identifier (e.g., "B04")
    * @param resolution Resolution in meters (10, 20, or 60)
    * @return Download URL for the band
    */
  def getBandDownloadUrl(
      productId: String,
      band: String,
      resolution: Int
  ): String = {
    // Copernicus Zipper service extracts specific files from products
    // Format: Products(uuid)/Nodes(path)/Nodes(filename)/$value
    val bandPath = s"GRANULE/.*/IMG_DATA/R${resolution}m/${band}_${resolution}m.jp2"
    
    // Note: This is a simplified approach. The actual implementation may need
    // to query the product structure first to get the exact file path
    s"$ODATA_BASE/Products($productId)/$$value"
  }

  /** Download a file from Copernicus with authentication
    *
    * @param url Download URL
    * @param outputPath Output file path
    * @param accessToken OAuth access token
    * @return Success or failure
    */
  def downloadFile(
      url: String,
      outputPath: java.nio.file.Path,
      accessToken: String
  ): Try[Unit] = Try {
    println(s"  Downloading from: $url")
    
    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("Authorization", s"Bearer $accessToken")
      .GET()
      .build()
    
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofFile(outputPath))
    
    if (response.statusCode() == 200) {
      val fileSize = java.nio.file.Files.size(outputPath)
      println(s"  ✓ Downloaded ${fileSize / 1_000_000} MB")
    } else {
      // Try to read error response body for debugging
      try {
        val errorBody = java.nio.file.Files.readString(outputPath)
        java.nio.file.Files.delete(outputPath)
        throw new RuntimeException(
          s"Download failed: HTTP ${response.statusCode()}\n" +
          s"Error response: ${errorBody.take(500)}"
        )
      } catch {
        case e: Exception =>
          if (java.nio.file.Files.exists(outputPath)) {
            java.nio.file.Files.delete(outputPath)
          }
          throw new RuntimeException(s"Download failed: HTTP ${response.statusCode()}")
      }
    }
  }

