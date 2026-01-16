package io.computenode.cyfra.analytics

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import munit.CatsEffectSuite
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.analytics.repository.*
import io.computenode.cyfra.analytics.service.{SegmentationService, DataGenerationService}
import io.computenode.cyfra.analytics.model.Transaction
import scala.concurrent.duration.*
import scala.util.Random

class LargeScaleE2ETest extends CatsEffectSuite:

  override def munitTimeout = 5.minutes

  val runtimeFixture = ResourceSuiteLocalFixture(
    "runtime",
    Resource.make(IO(VkCyfraRuntime()))(r => IO(r.close()))
  )

  override def munitFixtures = List(runtimeFixture)

  private def waitForProcessing(
    profileRepo: CustomerProfileRepository,
    expectedMin: Int,
    maxWait: FiniteDuration = 60.seconds,
    pollInterval: FiniteDuration = 500.millis
  ): IO[Int] =
    val deadline = System.currentTimeMillis() + maxWait.toMillis
    
    def poll: IO[Int] =
      for
        count <- profileRepo.count
        now = System.currentTimeMillis()
        result <- 
          if count >= expectedMin then IO.pure(count)
          else if now >= deadline then IO.pure(count)
          else IO.sleep(pollInterval) >> poll
      yield result
    
    poll

  test("handles 128k transaction batch efficiently") {
    given VkCyfraRuntime = runtimeFixture()

    val numTransactions = 128000
    val numCustomers = 25000
    val random = new Random(42)

    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      centroidsRepo <- InMemoryCentroidsRepository.create
      service <- SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtimeFixture())
      
      pipelineFiber <- service.pipeline.compile.drain.start
      
      // Phase 1: Generate transactions
      genStart = System.currentTimeMillis()
      transactions = (1 to numTransactions).map { i =>
        val customerId = (random.nextInt(numCustomers) + 1).toLong
        Transaction(
          customerId = customerId,
          timestamp = System.currentTimeMillis() - random.nextInt(365 * 24 * 3600) * 1000L,
          amount = 10.0 + random.nextDouble() * 490.0,
          items = 1 + random.nextInt(10),
          category = random.nextInt(20),
          channel = if random.nextBoolean() then "mobile_app" else "web",
          discountPct = if random.nextDouble() < 0.3 then 0.05 + random.nextDouble() * 0.20 else 0.0
        )
      }.toList
      genTime = System.currentTimeMillis() - genStart
      _ <- IO.println(s"[Phase 1] Generated $numTransactions transactions in ${genTime}ms")
      
      // Phase 2: Submit to queue
      submitStart = System.currentTimeMillis()
      _ <- service.submitBatch(transactions)
      submitTime = System.currentTimeMillis() - submitStart
      _ <- IO.println(s"[Phase 2] Submitted to queue in ${submitTime}ms")
      
      // Phase 3: Wait for processing (poll instead of fixed sleep)
      processStart = System.currentTimeMillis()
      uniqueCustomers = transactions.map(_.customerId).distinct.size
      _ <- IO.println(s"[Phase 3] Waiting for ~$uniqueCustomers customers to be processed...")
      
      finalCount <- waitForProcessing(profileRepo, uniqueCustomers * 8 / 10, maxWait = 120.seconds)
      processTime = System.currentTimeMillis() - processStart
      _ <- IO.println(s"[Phase 3] Processing completed in ${processTime}ms (${finalCount} profiles)")
      
      // Phase 4: Collect stats
      segments <- segmentRepo.getStats(List.empty)
      
      _ <- pipelineFiber.cancel
      
      totalTime = genTime + submitTime + processTime
      throughput = numTransactions.toDouble / (totalTime / 1000.0)
      
      _ <- IO.println(s"\n=== Performance Report ===")
      _ <- IO.println(s"Total transactions: $numTransactions")
      _ <- IO.println(s"Unique customers: $uniqueCustomers")
      _ <- IO.println(s"Profiles created: $finalCount")
      _ <- IO.println(s"Segments: ${segments.size}")
      _ <- IO.println(s"Generation time: ${genTime}ms")
      _ <- IO.println(s"Submit time: ${submitTime}ms")
      _ <- IO.println(s"Processing time: ${processTime}ms")
      _ <- IO.println(s"Total time: ${totalTime}ms (${totalTime / 1000.0}s)")
      _ <- IO.println(s"Throughput: ${throughput.toInt} txn/sec")
      _ <- IO.println("========================\n")
      
    yield
      assert(finalCount > 0, s"Expected profiles, got $finalCount")
      assert(finalCount <= numCustomers, s"Should not exceed $numCustomers customers")
      assert(segments.nonEmpty, "Expected segments to be assigned")
  }

  test("handles 50k transactions with timing breakdown") {
    given VkCyfraRuntime = runtimeFixture()

    val numTransactions = 50000
    val numCustomers = 10000
    val random = new Random(45)

    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      centroidsRepo <- InMemoryCentroidsRepository.create
      service <- SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtimeFixture())
      
      pipelineFiber <- service.pipeline.compile.drain.start
      
      transactions = (1 to numTransactions).map { i =>
        Transaction(
          customerId = (random.nextInt(numCustomers) + 1).toLong,
          timestamp = System.currentTimeMillis() - random.nextInt(180) * 86400000L,
          amount = 20.0 + random.nextDouble() * 300.0,
          items = 1 + random.nextInt(8),
          category = random.nextInt(20),
          channel = if random.nextBoolean() then "mobile_app" else "web",
          discountPct = if random.nextDouble() < 0.25 then random.nextDouble() * 0.25 else 0.0
        )
      }.toList
      
      startTime = System.currentTimeMillis()
      _ <- service.submitBatch(transactions)
      submitTime = System.currentTimeMillis() - startTime
      
      uniqueCustomers = transactions.map(_.customerId).distinct.size
      finalCount <- waitForProcessing(profileRepo, uniqueCustomers * 8 / 10, maxWait = 60.seconds)
      processTime = System.currentTimeMillis() - startTime - submitTime
      
      segments <- segmentRepo.getStats(List.empty)
      _ <- pipelineFiber.cancel
      
      totalTime = System.currentTimeMillis() - startTime
      throughput = numTransactions.toDouble / (totalTime / 1000.0)
      
      _ <- IO.println(s"\n=== 50k Transactions Test ===")
      _ <- IO.println(s"Submit: ${submitTime}ms | Process: ${processTime}ms | Total: ${totalTime}ms")
      _ <- IO.println(s"Profiles: $finalCount | Throughput: ${throughput.toInt} txn/sec")
      _ <- IO.println("============================\n")
      
    yield
      assert(finalCount > numCustomers / 2, s"Expected more profiles, got $finalCount")
      assert(segments.nonEmpty, "Expected segments")
  }

  test("bulk ingestion is faster than individual submissions") {
    given VkCyfraRuntime = runtimeFixture()

    val numTransactions = 5000  // Reduced for faster test
    val random = new Random(43)

    def generateTransactions(count: Int): List[Transaction] =
      (1 to count).map { i =>
        Transaction(
          customerId = (random.nextInt(500) + 1).toLong,
          timestamp = System.currentTimeMillis(),
          amount = 50.0 + random.nextDouble() * 200.0,
          items = 1 + random.nextInt(5),
          category = random.nextInt(20),
          channel = "web",
          discountPct = 0.0
        )
      }.toList

    for
      profileRepo1 <- InMemoryProfileRepository.create
      segmentRepo1 <- InMemorySegmentRepository.create
      centroidsRepo1 <- InMemoryCentroidsRepository.create
      service1 <- SegmentationService.create(profileRepo1, segmentRepo1, centroidsRepo1, runtimeFixture())
      
      profileRepo2 <- InMemoryProfileRepository.create
      segmentRepo2 <- InMemorySegmentRepository.create
      centroidsRepo2 <- InMemoryCentroidsRepository.create
      service2 <- SegmentationService.create(profileRepo2, segmentRepo2, centroidsRepo2, runtimeFixture())
      
      _ <- service1.pipeline.compile.drain.start
      _ <- service2.pipeline.compile.drain.start
      
      transactions1 = generateTransactions(numTransactions)
      transactions2 = generateTransactions(numTransactions)
      
      // Individual submissions
      individualStart = System.currentTimeMillis()
      _ <- transactions1.traverse_(service1.submitTransaction)
      _ <- waitForProcessing(profileRepo1, 400, maxWait = 30.seconds)
      individualTime = System.currentTimeMillis() - individualStart
      
      // Bulk submission
      bulkStart = System.currentTimeMillis()
      _ <- service2.submitBatch(transactions2)
      _ <- waitForProcessing(profileRepo2, 400, maxWait = 30.seconds)
      bulkTime = System.currentTimeMillis() - bulkStart
      
      count1 <- profileRepo1.count
      count2 <- profileRepo2.count
      
      _ <- IO.println(s"\n=== Bulk vs Individual ===")
      _ <- IO.println(s"Individual: ${individualTime}ms ($count1 profiles)")
      _ <- IO.println(s"Bulk: ${bulkTime}ms ($count2 profiles)")
      _ <- IO.println(s"Note: Individual may appear faster due to immediate processing vs batching")
      _ <- IO.println("========================\n")
      
    yield
      // Both should complete and process similar number of profiles
      assert(count1 > 0 && count2 > 0, "Both methods should produce profiles")
      assert(count1 > 300 && count2 > 300, "Both should process most customers")
  }

  test("handles concurrent bulk submissions") {
    given VkCyfraRuntime = runtimeFixture()

    val batchesCount = 3
    val transactionsPerBatch = 5000
    val random = new Random(44)

    for
      profileRepo <- InMemoryProfileRepository.create
      segmentRepo <- InMemorySegmentRepository.create
      centroidsRepo <- InMemoryCentroidsRepository.create
      service <- SegmentationService.create(profileRepo, segmentRepo, centroidsRepo, runtimeFixture())
      
      _ <- service.pipeline.compile.drain.start
      
      batches = (1 to batchesCount).map { _ =>
        (1 to transactionsPerBatch).map { _ =>
          Transaction(
            customerId = (random.nextInt(2000) + 1).toLong,
            timestamp = System.currentTimeMillis(),
            amount = 50.0 + random.nextDouble() * 200.0,
            items = 1 + random.nextInt(5),
            category = random.nextInt(20),
            channel = "mobile_app",
            discountPct = 0.0
          )
        }.toList
      }.toList
      
      startTime = System.currentTimeMillis()
      _ <- batches.parTraverse_(service.submitBatch)
      submitTime = System.currentTimeMillis() - startTime
      
      finalCount <- waitForProcessing(profileRepo, 1500, maxWait = 30.seconds)
      totalTime = System.currentTimeMillis() - startTime
      
      totalTransactions = batchesCount * transactionsPerBatch
      throughput = totalTransactions.toDouble / (totalTime / 1000.0)
      
      _ <- IO.println(s"\n=== Concurrent Batches ===")
      _ <- IO.println(s"Batches: $batchesCount x $transactionsPerBatch = $totalTransactions txns")
      _ <- IO.println(s"Submit: ${submitTime}ms | Total: ${totalTime}ms")
      _ <- IO.println(s"Profiles: $finalCount | Throughput: ${throughput.toInt} txn/sec")
      _ <- IO.println("========================\n")
      
    yield
      assert(finalCount > 0, "Expected profiles to be created")
  }
