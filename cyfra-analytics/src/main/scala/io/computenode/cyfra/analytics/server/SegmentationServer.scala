package io.computenode.cyfra.analytics.server

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.analytics.repository.*
import io.computenode.cyfra.analytics.service.{SegmentationService, DataGenerationService, FeatureExtractionService}
import io.computenode.cyfra.analytics.endpoints.SegmentationEndpoints
import io.computenode.cyfra.utility.Logger.logger

/** GPU-accelerated Customer Segmentation Server.
  *
  * Demonstrates Cyfra GPU compute integrated with fs2 streaming:
  *   - 100,000 customers
  *   - 100 behavioral features
  *   - 50 customer segments
  *   - ~500,000 customers/second GPU throughput
  */
object SegmentationServer extends IOApp.Simple:

  val NumCustomers = 100_000
  val TransactionsPerCustomer = 20

  def run: IO[Unit] =
    resources.use { case (service, endpoints, dataGen) =>
      val routes = Http4sServerInterpreter[IO]().toRoutes(endpoints.serverEndpoints)
      val swaggerRoutes =
        Http4sServerInterpreter[IO]().toRoutes(SwaggerInterpreter().fromEndpoints[IO](endpoints.all, "GPU Customer Segmentation API", "1.0.0"))

      val serverResource = EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8081")
        .withHttpApp((routes <+> swaggerRoutes).orNotFound)
        .build

      for
        _ <- IO(logger.info(""))
        _ <- IO(logger.info("GPU-Accelerated Customer Segmentation Service"))
        _ <- IO(logger.info(s"  Customers: $NumCustomers"))
        _ <- IO(logger.info(s"  Features:  ${FeatureExtractionService.NumFeatures}"))
        _ <- IO(logger.info(s"  Segments:  ${FeatureExtractionService.NumClusters}"))
        _ <- IO(logger.info(""))

        startGen <- IO(System.currentTimeMillis())
        _ <- dataGen.generateSampleData(NumCustomers, TransactionsPerCustomer)
        genTime = System.currentTimeMillis() - startGen
        _ <- IO(logger.info(f"Generated $NumCustomers%,d customers in ${genTime / 1000.0}%.1fs"))

        _ <- service.pipeline.compile.drain.start
        _ <- IO(logger.info("Started fs2 + GPU segmentation pipeline"))

        _ <- serverResource.use { _ =>
          IO(logger.info(s"Server: http://localhost:8081")) >> IO(logger.info(s"Swagger: http://localhost:8081/docs")) >> IO.never
        }
      yield ()
    }

  private def resources: Resource[IO, (SegmentationService, SegmentationEndpoints, DataGenerationService)] =
    for
      runtime <- Resource.make(IO(VkCyfraRuntime()))(r => IO(r.close()))
      profileRepo <- Resource.eval(InMemoryProfileRepository.create)
      segmentRepo <- Resource.eval(InMemorySegmentRepository.create)
      centroidsRepo <- Resource.eval(InMemoryCentroidsRepository.create)
      service <- Resource.eval(SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtime))
      dataGen <- Resource.eval(DataGenerationService.create(profileRepo, segmentRepo))
    yield (service, new SegmentationEndpoints(service, centroidsRepo), dataGen)
