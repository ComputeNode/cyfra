package io.computenode.cyfra.analytics.service

import cats.effect.IO
import cats.syntax.foldable.*
import io.computenode.cyfra.analytics.model.{Transaction, CustomerProfile}
import io.computenode.cyfra.analytics.repository.{CustomerProfileRepository, CustomerSegmentRepository}
import scala.util.Random

/** Generates synthetic customer data for demonstration. */
class DataGenerationService(profileRepo: CustomerProfileRepository, segmentRepo: CustomerSegmentRepository):

  def generateSampleData(numCustomers: Int = 100_000, transactionsPerCustomer: Int = 20): IO[Unit] =
    val random = new Random(42)
    val now = System.currentTimeMillis()
    val day = 86400000L
    val batchSize = 10_000

    (1 to numCustomers).grouped(batchSize).toList.traverse_ { customerIds =>
      val customers = customerIds.toList.map { customerId =>
        val txs = generateTransactions(customerId, transactionsPerCustomer, now, day, random)
        CustomerProfile(
          customerId = customerId.toLong,
          features = FeatureExtractionService.computeFeatures(txs),
          transactionCount = txs.size,
          lastUpdate = now,
          totalSpend = txs.map(_.amount).sum,
          lastTransactionDays = ((now - txs.map(_.timestamp).max) / day).toInt,
        )
      }
      profileRepo.upsertBatch(customers)
    }

  private def generateTransactions(customerId: Int, count: Int, now: Long, day: Long, random: Random): List[Transaction] =
    val archetype = customerId % 50
    val category = archetype / 5

    (1 to count).toList.map { _ =>
      val daysAgo = category match
        case 0 | 1 | 2 | 3 => random.nextInt(30) + 1
        case 7             => random.nextInt(180) + 90
        case _             => random.nextInt(90) + 1

      val amount = category match
        case 0 => 200.0 + random.nextDouble() * 800.0
        case 1 => 80.0 + random.nextDouble() * 120.0
        case 2 => 50.0 + random.nextDouble() * 150.0
        case 3 => 60.0 + random.nextDouble() * 100.0
        case 4 => 30.0 + random.nextDouble() * 50.0
        case 5 => 50.0 + random.nextDouble() * 200.0
        case 6 => 40.0 + random.nextDouble() * 80.0
        case 7 => 30.0 + random.nextDouble() * 70.0
        case 8 => 100.0 + random.nextDouble() * 300.0
        case _ => 40.0 + random.nextDouble() * 160.0

      Transaction(
        customerId = customerId.toLong,
        timestamp = now - (daysAgo * day) + random.nextInt(86400000),
        amount = amount,
        items = 1 + random.nextInt(if category == 0 || category == 8 then 8 else 5),
        category = random.nextInt(20),
        channel = if category == 3 && random.nextDouble() < 0.7 then "mobile_app" else if random.nextBoolean() then "mobile_app" else "web",
        discountPct =
          if category == 4 then random.nextDouble() * 0.4 else if category == 0 then random.nextDouble() * 0.05 else random.nextDouble() * 0.15,
      )
    }

object DataGenerationService:
  def create(profileRepo: CustomerProfileRepository, segmentRepo: CustomerSegmentRepository): IO[DataGenerationService] =
    IO.pure(new DataGenerationService(profileRepo, segmentRepo))
