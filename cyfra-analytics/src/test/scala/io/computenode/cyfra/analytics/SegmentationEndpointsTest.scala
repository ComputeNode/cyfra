package io.computenode.cyfra.analytics

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.circe.CirceEntityCodec.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.analytics.repository.*
import io.computenode.cyfra.analytics.service.{SegmentationService, DataGenerationService}
import io.computenode.cyfra.analytics.endpoints.SegmentationEndpoints
import io.computenode.cyfra.analytics.model.*
import io.computenode.cyfra.analytics.service.FeatureExtractionService
import scala.concurrent.duration.*

class SegmentationEndpointsTest extends CatsEffectSuite:

  val runtimeFixture = ResourceSuiteLocalFixture(
    "runtime",
    Resource.make(IO(VkCyfraRuntime()))(r => IO(r.close()))
  )

  override def munitFixtures = List(runtimeFixture)

  def setupService(using VkCyfraRuntime): IO[(SegmentationService, HttpApp[IO], DataGenerationService)] =
    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      centroidsRepo <- InMemoryCentroidsRepository.create
      
      service <- SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtimeFixture())
      endpoints = new SegmentationEndpoints(service, centroidsRepo)
      dataGen <- DataGenerationService.create(profileRepo, segmentRepo)
      
      routes = Http4sServerInterpreter[IO]().toRoutes(endpoints.serverEndpoints)
      
      _ <- service.pipeline.compile.drain.start
    yield (service, routes.orNotFound, dataGen)

  test("GET /health returns service status") {
    given VkCyfraRuntime = runtimeFixture()

    setupService.flatMap { (_, app, _) =>
      val request = Request[IO](Method.GET, uri"/health")
      
      for
        response <- app.run(request)
        body <- response.as[HealthStatus]
      yield
        assertEquals(response.status, Status.Ok)
        assertEquals(body.status, "ok")
        assertEquals(body.clustersActive, FeatureExtractionService.NumClusters)
    }
  }

  test("GET /api/v1/segments returns segment statistics") {
    given VkCyfraRuntime = runtimeFixture()

    setupService.flatMap { (_, app, dataGen) =>
      for
        _ <- dataGen.generateSampleData(numCustomers = 100, transactionsPerCustomer = 20)
        _ <- IO.sleep(1.second)
        
        request = Request[IO](Method.GET, uri"/api/v1/segments")
        response <- app.run(request)
        body <- response.as[SegmentsResponse]
      yield
        assertEquals(response.status, Status.Ok)
        assertEquals(body.totalCustomers, 100)
    }
  }

  test("GET /api/v1/centroids returns current centroids") {
    given VkCyfraRuntime = runtimeFixture()

    setupService.flatMap { (_, app, _) =>
      val request = Request[IO](Method.GET, uri"/api/v1/centroids")
      
      for
        response <- app.run(request)
        body <- response.as[CentroidsResponse]
      yield
        assertEquals(response.status, Status.Ok)
        assertEquals(body.numClusters, FeatureExtractionService.NumClusters)
        assertEquals(body.numFeatures, FeatureExtractionService.NumFeatures)
        assertEquals(body.labels.size, FeatureExtractionService.NumClusters)
    }
  }
