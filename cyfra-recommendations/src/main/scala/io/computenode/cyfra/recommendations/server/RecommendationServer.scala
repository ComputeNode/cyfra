package io.computenode.cyfra.recommendations.server

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.recommendations.endpoints.RecommendationEndpoints
import io.computenode.cyfra.recommendations.service.RecommendationService

/** HTTP Server for GPU-accelerated product recommendations
  *
  * Endpoints:
  * - GET  /api/v1/health                    - Health check
  * - GET  /api/v1/products/{id}             - Get product by ID
  * - GET  /api/v1/products                  - List products
  * - POST /api/v1/recommendations           - Get recommendations
  * - GET  /docs                              - Swagger UI
  *
  * Usage:
  *   sbt "project recommendations" "runMain io.computenode.cyfra.recommendations.server.RecommendationServer"
  */
object RecommendationServer extends IOApp.Simple:
  
  private val ServerHost = host"0.0.0.0"
  private val ServerPort = port"8080"
  
  def run: IO[Unit] =
    // Initialize Vulkan/GPU runtime
    Resource.make(IO(VkCyfraRuntime()))(rt => IO(rt.close())).use { implicit runtime =>
      for
        _ <- IO.println("=" * 70)
        _ <- IO.println("Cyfra GPU-Accelerated Recommendation Service")
        _ <- IO.println("=" * 70)
        
        // Initialize recommendation service with demo data
        _ <- IO.println("Initializing recommendation service...")
        service <- RecommendationService.createDemo(numProducts = 1000, embeddingDim = 128)
        
        _ <- IO.println(s"Loaded ${service.getAllProducts.size} products with GPU acceleration")
        _ <- IO.println("=" * 70)
        
        // Build and start HTTP server
        _ <- createServer(service).use { server =>
          IO.println(s"Server starting on http://$ServerHost:$ServerPort") *>
          IO.println(s"API docs available at http://$ServerHost:$ServerPort/docs") *>
          IO.println("=" * 70) *>
          IO.println("Press Ctrl+C to stop") *>
          IO.println("") *>
          IO.never  // Keep server running forever
        }
      yield ()
    }
  
  private def createServer(service: RecommendationService)(using runtime: VkCyfraRuntime): Resource[IO, Server] =
    val apiEndpoints = RecommendationEndpoints.serverEndpoints(service)
    
    // Generate Swagger UI with explicit configuration
    val swaggerEndpoints = SwaggerInterpreter()
      .fromServerEndpoints[IO](
        apiEndpoints, 
        "Cyfra GPU-Accelerated Recommendations",
        "1.0.0"
      )
    
    val allEndpoints = apiEndpoints ++ swaggerEndpoints
    
    val routes = Http4sServerInterpreter[IO]().toRoutes(allEndpoints)
    
    EmberServerBuilder
      .default[IO]
      .withHost(ServerHost)
      .withPort(ServerPort)
      .withHttpApp(routes.orNotFound)
      .build
