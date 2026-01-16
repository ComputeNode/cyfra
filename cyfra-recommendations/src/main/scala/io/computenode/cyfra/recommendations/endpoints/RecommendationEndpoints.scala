package io.computenode.cyfra.recommendations.endpoints

import cats.effect.IO
import io.computenode.cyfra.recommendations.model.*
import io.computenode.cyfra.recommendations.service.RecommendationService
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Tapir endpoint definitions for recommendation API */
object RecommendationEndpoints:
  
  // Base path for all recommendation endpoints
  private val baseEndpoint = endpoint
    .in("api" / "v1")
    .errorOut(jsonBody[ErrorResponse])
  
  // ============================================================================
  // Endpoint Definitions
  // ============================================================================
  
  /** GET /api/v1/health - Health check endpoint */
  val healthEndpoint: PublicEndpoint[Unit, ErrorResponse, HealthStatus, Any] =
    baseEndpoint.get
      .in("health")
      .out(jsonBody[HealthStatus])
      .description("Check service health and GPU availability")
      .tag("System")
  
  /** GET /api/v1/products/{id} - Get product by ID */
  val getProductEndpoint: PublicEndpoint[Long, ErrorResponse, Product, Any] =
    baseEndpoint.get
      .in("products" / path[Long]("productId"))
      .out(jsonBody[Product])
      .description("Retrieve a product by its ID")
      .tag("Products")
  
  /** GET /api/v1/products - List all products */
  val listProductsEndpoint: PublicEndpoint[(Option[Int], Option[Int]), ErrorResponse, List[Product], Any] =
    baseEndpoint.get
      .in("products")
      .in(query[Option[Int]]("offset").description("Offset for pagination"))
      .in(query[Option[Int]]("limit").description("Limit for pagination (max 100)"))
      .out(jsonBody[List[Product]])
      .description("List products with optional pagination")
      .tag("Products")
  
  /** POST /api/v1/recommendations - Get product recommendations */
  val recommendationsEndpoint: PublicEndpoint[RecommendationRequest, ErrorResponse, RecommendationResponse, Any] =
    baseEndpoint.post
      .in("recommendations")
      .in(jsonBody[RecommendationRequest])
      .out(jsonBody[RecommendationResponse])
      .description("Get similar product recommendations using GPU-accelerated similarity search")
      .tag("Recommendations")
  
  // ============================================================================
  // Server Logic (binding endpoints to service)
  // ============================================================================
  
  /** Create server endpoints with service logic */
  def serverEndpoints(service: RecommendationService): List[ServerEndpoint[Any, IO]] =
    List(
      healthServerEndpoint(service),
      getProductServerEndpoint(service),
      listProductsServerEndpoint(service),
      recommendationsServerEndpoint(service)
    )
  
  private def healthServerEndpoint(service: RecommendationService): ServerEndpoint[Any, IO] =
    healthEndpoint.serverLogicSuccess { _ =>
      IO {
        val (numProducts, embeddingDim, gpuAvailable) = service.getStats
        HealthStatus(
          status = "ok",
          gpuAvailable = gpuAvailable,
          productsLoaded = numProducts,
          embeddingDimension = embeddingDim
        )
      }
    }
  
  private def getProductServerEndpoint(service: RecommendationService): ServerEndpoint[Any, IO] =
    getProductEndpoint.serverLogic { productId =>
      IO {
        service.getProduct(productId) match
          case Some(product) => Right(product)
          case None => Left(ErrorResponse("not_found", s"Product $productId not found"))
      }
    }
  
  private def listProductsServerEndpoint(service: RecommendationService): ServerEndpoint[Any, IO] =
    listProductsEndpoint.serverLogic { case (offsetOpt, limitOpt) =>
      IO {
        val offset = offsetOpt.getOrElse(0)
        val limit = math.min(limitOpt.getOrElse(20), 100)
        
        val products = service.getAllProducts
          .drop(offset)
          .take(limit)
          .toList
        
        Right(products)
      }
    }
  
  private def recommendationsServerEndpoint(service: RecommendationService): ServerEndpoint[Any, IO] =
    recommendationsEndpoint.serverLogic { request =>
      service.findSimilar(
        productId = request.productId,
        limit = request.limit.getOrElse(10),
        minScore = request.minScore.getOrElse(0.0f)
      ).map { case (recommendations, computeTime) =>
        Right(RecommendationResponse(
          requestedProductId = request.productId,
          recommendations = recommendations,
          computeTimeMs = computeTime
        ))
      }.handleErrorWith { error =>
        IO.pure(Left(ErrorResponse(
          error = "recommendation_error",
          message = error.getMessage
        )))
      }
    }
