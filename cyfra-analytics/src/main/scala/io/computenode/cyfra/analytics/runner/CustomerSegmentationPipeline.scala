package io.computenode.cyfra.analytics.runner

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import fs2.{Stream, Pipe}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.fs2interop.GCluster
import io.computenode.cyfra.utility.Logger.logger

import scala.util.Random

/** E-commerce Customer Segmentation using fs2 + Cyfra GPU K-Means.
  *
  * Pipeline: Transactions → RFM Features → Normalize → GPU Cluster → Segments
  */
object CustomerSegmentationPipeline extends IOApp.Simple:

  // Domain
  case class Transaction(customerId: Long, timestamp: Long, amount: Double, items: Int, category: Int)
  case class CustomerRFM(id: Long, recency: Float, frequency: Float, monetary: Float, avgOrder: Float, avgItems: Float, diversity: Float)

  // Config
  val NumCustomers = 5000
  val EventsPerCustomer = 20
  val NumClusters = 8
  val NumFeatures = 6

  val Segments = Vector("VIP Champions", "Loyal", "Potential Loyalists", "New", "Promising", "Need Attention", "About to Sleep", "At Risk")

  // Simulate transactions with 8 customer archetypes
  def transactions: Stream[IO, Transaction] =
    val random = new Random(42)
    val now = System.currentTimeMillis()
    val day = 86400000L
    val archetypes = Vector((0.1, 500.0, 10, 8), (0.3, 200.0, 5, 6), (0.5, 100.0, 3, 4), (2.0, 50.0, 2, 3),
                            (1.0, 80.0, 3, 5), (3.0, 60.0, 2, 2), (6.0, 40.0, 1, 1), (12.0, 30.0, 1, 1))
    Stream.emits(
      for
        cid <- 0 until NumCustomers
        (recF, baseAmt, maxItems, maxCats) = archetypes(cid % 8)
        _ <- 0 until EventsPerCustomer
        noise = 0.5 + random.nextDouble()
      yield Transaction(cid, now - (random.nextDouble() * 30 * recF * noise * day).toLong.max(day),
                        baseAmt * noise * (0.5 + random.nextDouble()), 1 + random.nextInt(maxItems), random.nextInt(maxCats.max(1)))
    ).covary[IO]

  // Aggregate to RFM features
  def toRFM: Pipe[IO, Transaction, CustomerRFM] =
    _.groupAdjacentBy(_.customerId).map { case (id, chunk) =>
      val txs = chunk.toList
      val now = System.currentTimeMillis()
      val day = 86400000f
      CustomerRFM(id,
        (now - txs.map(_.timestamp).max) / day,
        txs.size / 12f,
        (txs.map(_.amount).sum / txs.size).toFloat,
        txs.map(_.amount).sum.toFloat,
        txs.map(_.items).sum.toFloat / txs.size,
        txs.map(_.category).distinct.size / 10f)
    }

  // Normalize features to [0,1]
  def normalize(rfms: List[CustomerRFM]): List[(Long, Array[Float])] =
    def minMax(f: CustomerRFM => Float) = (rfms.map(f).min, rfms.map(f).max)
    def norm(v: Float, mm: (Float, Float)) = if mm._2 - mm._1 < 0.0001f then 0.5f else (v - mm._1) / (mm._2 - mm._1)
    val (recMM, freqMM, monMM, ltv, items, div) = (minMax(_.recency), minMax(_.frequency), minMax(_.monetary),
                                                   minMax(_.avgOrder), minMax(_.avgItems), minMax(_.diversity))
    rfms.map(r => (r.id, Array(norm(r.recency, recMM), norm(r.frequency, freqMM), norm(r.monetary, monMM),
                               norm(r.avgOrder, ltv), norm(r.avgItems, items), norm(r.diversity, div))))

  // GPU K-Means clustering
  def cluster(using VkCyfraRuntime): Pipe[IO, (Long, Array[Float]), (Long, Int)] =
    GCluster.kMeansTyped(GCluster.KMeansConfig(NumClusters, NumFeatures, numIterations = 20, batchSize = 1024),
      _._2, (in, c) => (in._1, c))

  def run: IO[Unit] =
    Resource.make(IO(VkCyfraRuntime()))(r => IO(r.close())).use { implicit runtime =>
      for
        start <- IO(System.nanoTime())

        // Full pipeline: transactions → RFM → normalize → GPU cluster
        rfms <- transactions.through(toRFM).compile.toList
        segments <- Stream.emits(normalize(rfms)).through(cluster).compile.toList

        elapsed = (System.nanoTime() - start) / 1e6

        // Results
        counts = segments.groupBy(_._2).view.mapValues(_.size).toMap
        _ <- IO {
          logger.info("=" * 60)
          logger.info("E-COMMERCE CUSTOMER SEGMENTATION")
          logger.info("=" * 60)
          logger.info(f"$NumCustomers customers clustered into $NumClusters segments in $elapsed%.0f ms")
          logger.info("")
          (0 until NumClusters).foreach { id =>
            val n = counts.getOrElse(id, 0)
            logger.info(f"  $id: ${Segments(id)}%-20s  $n%5d (${n * 100.0 / segments.size}%5.1f%%)")
          }
          logger.info("=" * 60)
        }
      yield ()
    }
