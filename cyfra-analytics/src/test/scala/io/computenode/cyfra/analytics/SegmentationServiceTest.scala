package io.computenode.cyfra.analytics

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import munit.CatsEffectSuite
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.analytics.repository.*
import io.computenode.cyfra.analytics.service.{SegmentationService, DataGenerationService}
import io.computenode.cyfra.analytics.model.Transaction
import io.computenode.cyfra.analytics.service.FeatureExtractionService
import scala.concurrent.duration.*

class SegmentationServiceTest extends CatsEffectSuite:

  val runtimeFixture = ResourceSuiteLocalFixture("runtime", Resource.make(IO(VkCyfraRuntime()))(r => IO(r.close())))

  override def munitFixtures = List(runtimeFixture)

  test("data generation creates sample customers") {
    given VkCyfraRuntime = runtimeFixture()

    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      dataGen <- DataGenerationService.create(profileRepo, segmentRepo)

      _ <- dataGen.generateSampleData(numCustomers = 50, transactionsPerCustomer = 10)

      count <- profileRepo.count
      profiles <- profileRepo.getAll
    yield
      assertEquals(count, 50)
      assert(profiles.forall(_.transactionCount == 10))
      assert(profiles.forall(_.features.length == FeatureExtractionService.NumFeatures))
  }

  test("continuous pipeline processes transactions and assigns segments") {
    given VkCyfraRuntime = runtimeFixture()

    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      centroidsRepo <- InMemoryCentroidsRepository.create
      service <- SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtimeFixture())

      _ <- service.pipeline.compile.drain.start

      transactions = (1 to 300).map { i =>
        Transaction(
          customerId = (i % 10) + 1,
          timestamp = System.currentTimeMillis() - (i * 1000000L),
          amount = 50.0 + (i % 100) * 5.0,
          items = 1 + (i % 5),
          category = i % 20,
          channel = if i % 3 == 0 then "mobile_app" else "web",
          discountPct = if i % 4 == 0 then 0.15 else 0.0,
        )
      }

      _ <- transactions.toList.traverse(service.submitTransaction)
      _ <- IO.sleep(3.seconds)

      count <- profileRepo.count
      segments <- segmentRepo.get(1)
    yield
      assert(count > 0, s"Expected profiles, got $count")
      assert(segments.isDefined, "Expected customer 1 to have segment")
  }
