package io.computenode.cyfra.recommendations

import cats.effect.{IO, Resource}
import io.circe.parser.*
import io.circe.syntax.*
import io.computenode.cyfra.recommendations.endpoints.RecommendationEndpoints
import io.computenode.cyfra.recommendations.model.*
import io.computenode.cyfra.recommendations.service.RecommendationService
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.implicits.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

class RecommendationEndpointsTest extends CatsEffectSuite:
  
  val runtimeResource: Resource[IO, VkCyfraRuntime] =
    Resource.make(IO(VkCyfraRuntime()))(rt => IO(rt.close()))
  
  val runtimeFixture = ResourceSuiteLocalFixture("runtime", runtimeResource)
  
  override def munitFixtures = List(runtimeFixture)
  
  // Helper to create routes for testing
  def createRoutes(using VkCyfraRuntime): IO[HttpRoutes[IO]] =
    RecommendationService.createDemo(numProducts = 50, embeddingDim = 128).map { service =>
      val endpoints = RecommendationEndpoints.serverEndpoints(service)
      Http4sServerInterpreter[IO]().toRoutes(endpoints)
    }
  
  test("GET /api/v1/health should return health status") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      routes <- createRoutes
      request = Request[IO](Method.GET, uri"/api/v1/health")
      response <- routes.orNotFound.run(request)
      healthStatus <- response.as[HealthStatus]
    yield
      assertEquals(response.status, Status.Ok)
      assertEquals(healthStatus.status, "ok")
      assert(healthStatus.gpuAvailable, "GPU should be available")
      assertEquals(healthStatus.productsLoaded, 50)
      assertEquals(healthStatus.embeddingDimension, 128)
  }
  
  test("GET /api/v1/products/{id} should return product") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      routes <- createRoutes
      request = Request[IO](Method.GET, uri"/api/v1/products/0")
      response <- routes.orNotFound.run(request)
      product <- response.as[Product]
    yield
      assertEquals(response.status, Status.Ok)
      assertEquals(product.id, 0L)
      assert(product.name.nonEmpty)
      assert(product.embedding.size == 128)
  }
  
  test("GET /api/v1/products/{id} should return 404 for non-existent product") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      routes <- createRoutes
      request = Request[IO](Method.GET, uri"/api/v1/products/9999")
      response <- routes.orNotFound.run(request)
      error <- response.as[ErrorResponse]
    yield
      assertEquals(response.status, Status.BadRequest) // Tapir returns 400 for errors
      assertEquals(error.error, "not_found")
      assert(error.message.contains("not found"))
  }
  
  test("GET /api/v1/products should return paginated products") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      routes <- createRoutes
      request = Request[IO](Method.GET, uri"/api/v1/products?offset=0&limit=10")
      response <- routes.orNotFound.run(request)
      products <- response.as[List[Product]]
    yield
      assertEquals(response.status, Status.Ok)
      assertEquals(products.size, 10)
      assert(products.forall(_.embedding.size == 128))
  }
  
  test("POST /api/v1/recommendations should return recommendations") {
    given VkCyfraRuntime = runtimeFixture()
    
    val requestBody = RecommendationRequest(
      productId = 0L,
      limit = Some(5),
      minScore = Some(0.0f)
    )
    
    for
      routes <- createRoutes
      request = Request[IO](Method.POST, uri"/api/v1/recommendations")
        .withEntity(requestBody.asJson)
      response <- routes.orNotFound.run(request)
      recommendations <- response.as[RecommendationResponse]
    yield
      assertEquals(response.status, Status.Ok)
      assertEquals(recommendations.requestedProductId, 0L)
      assert(recommendations.recommendations.nonEmpty, "Should return recommendations")
      assert(recommendations.recommendations.size <= 5, "Should respect limit")
      assert(recommendations.computeTimeMs >= 0, "Compute time should be non-negative")
      
      // Verify recommendations structure
      recommendations.recommendations.foreach { rec =>
        assert(rec.product.id != 0L, "Should not recommend the query product itself")
        assert(rec.similarityScore >= 0.0f && rec.similarityScore <= 1.0f, 
          "Similarity score should be in [0, 1]")
      }
  }
  
  test("POST /api/v1/recommendations should respect limit parameter") {
    given VkCyfraRuntime = runtimeFixture()
    
    val requestBody = RecommendationRequest(
      productId = 0L,
      limit = Some(3),
      minScore = None
    )
    
    for
      routes <- createRoutes
      request = Request[IO](Method.POST, uri"/api/v1/recommendations")
        .withEntity(requestBody.asJson)
      response <- routes.orNotFound.run(request)
      recommendations <- response.as[RecommendationResponse]
    yield
      assert(recommendations.recommendations.size <= 3, 
        s"Should return at most 3 recommendations, got ${recommendations.recommendations.size}")
  }
  
  test("POST /api/v1/recommendations should respect minScore threshold") {
    given VkCyfraRuntime = runtimeFixture()
    
    val requestBody = RecommendationRequest(
      productId = 0L,
      limit = Some(10),
      minScore = Some(0.8f)  // High threshold
    )
    
    for
      routes <- createRoutes
      request = Request[IO](Method.POST, uri"/api/v1/recommendations")
        .withEntity(requestBody.asJson)
      response <- routes.orNotFound.run(request)
      recommendations <- response.as[RecommendationResponse]
    yield
      // All returned recommendations should meet threshold
      recommendations.recommendations.foreach { rec =>
        assert(rec.similarityScore >= 0.8f, 
          s"Similarity score ${rec.similarityScore} should be >= 0.8")
      }
  }
  
  test("POST /api/v1/recommendations should handle non-existent product") {
    given VkCyfraRuntime = runtimeFixture()
    
    val requestBody = RecommendationRequest(
      productId = 9999L,
      limit = Some(10),
      minScore = None
    )
    
    for
      routes <- createRoutes
      request = Request[IO](Method.POST, uri"/api/v1/recommendations")
        .withEntity(requestBody.asJson)
      response <- routes.orNotFound.run(request)
      error <- response.as[ErrorResponse]
    yield
      assertEquals(response.status, Status.BadRequest) // Tapir returns 400 for errors
      assertEquals(error.error, "recommendation_error")
      assert(error.message.contains("not found"))
  }
