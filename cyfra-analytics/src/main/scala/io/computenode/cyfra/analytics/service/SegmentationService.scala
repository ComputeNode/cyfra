package io.computenode.cyfra.analytics.service

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.{Chunk, Pipe, Stream}
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.analytics.gpu.GpuAnalyticsPipeline
import io.computenode.cyfra.analytics.gpu.GpuAnalyticsPipeline.{Config, SegmentResult}
import io.computenode.cyfra.analytics.model.*
import io.computenode.cyfra.analytics.repository.*
import io.computenode.cyfra.fs2interop.GCluster.FCMCentroids
import scala.concurrent.duration.*

/** GPU-accelerated customer segmentation as an fs2 streaming pipeline.
  *
  * Demonstrates seamless integration of Cyfra GPU compute with fs2 streams:
  *   - Transactions flow in via queue
  *   - Batched and aggregated into customer profiles
  *   - GPU pipe directly in stream computes fuzzy memberships
  *   - Results stream out to repositories
  */
class SegmentationService(
  profileRepo: CustomerProfileRepository,
  segmentRepo: CustomerSegmentRepository,
  centroidsRepo: CentroidsRepository,
  runtime: VkCyfraRuntime,
  transactionQueue: Queue[IO, Transaction],
  lastUpdateRef: Ref[IO, Long],
  transactionCache: Ref[IO, Map[Long, List[Transaction]]],
  centroidsCache: Ref[IO, (FCMCentroids, Vector[String])],
):
  private given VkCyfraRuntime = runtime

  private val BatchSize = 10_000
  private val BatchTimeout = 500.millis
  private val MaxCachedTransactions = 50

  def submitTransaction(tx: Transaction): IO[Unit] =
    transactionQueue.offer(tx)

  def submitBatch(transactions: List[Transaction]): IO[Unit] =
    transactions.traverse_(transactionQueue.offer)

  /** Main processing pipeline: transactions → profiles → GPU segmentation → storage
    *
    * The GPU pipe (GpuAnalyticsPipeline.segment) sits directly in the stream. Profiles flow as chunks through the GPU for parallel membership
    * computation.
    */
  def pipeline: Stream[IO, Unit] =
    Stream.eval(refreshCentroidsCache) ++
      Stream
        .fromQueueUnterminated(transactionQueue)
        .groupWithin(BatchSize, BatchTimeout)
        .through(cacheTransactions)
        .through(extractProfiles)
        .unchunks
        .through(gpuSegment)
        .through(toSegmentResult)
        .groupWithin(BatchSize, BatchTimeout)
        .through(persistResults)

  private def refreshCentroidsCache: IO[Unit] =
    (centroidsRepo.get, centroidsRepo.getLabels).tupled.flatMap(centroidsCache.set)

  private def cacheTransactions: Pipe[IO, Chunk[Transaction], Chunk[Transaction]] =
    _.evalTap { chunk =>
      transactionCache.update { cache =>
        chunk.toList.groupBy(_.customerId).foldLeft(cache) { case (c, (id, txs)) =>
          c.updated(id, (txs ++ c.getOrElse(id, Nil)).distinctBy(_.timestamp).take(MaxCachedTransactions))
        }
      }
    }

  private def extractProfiles: Pipe[IO, Chunk[Transaction], Chunk[CustomerProfile]] =
    _.evalMap { chunk =>
      transactionCache.get.map { cache =>
        Chunk.from(chunk.toList.groupBy(_.customerId).toList.map { case (customerId, txs) =>
          val allTxs = (txs ++ cache.getOrElse(customerId, Nil)).distinctBy(_.timestamp).take(MaxCachedTransactions)
          CustomerProfile(
            customerId = customerId,
            features = FeatureExtractionService.computeFeatures(allTxs),
            transactionCount = allTxs.size,
            lastUpdate = System.currentTimeMillis(),
            totalSpend = allTxs.map(_.amount).sum,
            lastTransactionDays = ((System.currentTimeMillis() - allTxs.map(_.timestamp).max) / 86400000.0).toInt,
          )
        })
      }
    }

  /** GPU pipe directly in stream - Cyfra integration point.
    *
    * Uses groupWithin for time-bounded batching so small batches don't block.
    */
  private def gpuSegment: Pipe[IO, CustomerProfile, SegmentResult] =
    _.groupWithin(BatchSize, 100.millis).flatMap { chunk =>
      Stream.eval(centroidsCache.get).flatMap { case (centroids, _) =>
        val config = Config(
          numFeatures = FeatureExtractionService.NumFeatures,
          numClusters = FeatureExtractionService.NumClusters,
          batchSize = Math.max(chunk.size, 1024),
        )
        Stream.chunk(chunk).through(GpuAnalyticsPipeline.segment(config, centroids))
      }
    }

  private def toSegmentResult: Pipe[IO, SegmentResult, (CustomerProfile, CustomerSegment)] =
    _.evalMap { result =>
      centroidsCache.get.map { case (_, labels) =>
        val segment = CustomerSegment(
          customerId = result.customer.customerId,
          segmentName = labels(result.dominantSegment),
          confidence = result.dominantWeight,
          memberships = result.membership.values.zipWithIndex.map { case (w, i) => labels(i) -> w }.toMap,
          assignedAt = System.currentTimeMillis(),
        )
        (result.customer, segment)
      }
    }

  private def persistResults: Pipe[IO, Chunk[(CustomerProfile, CustomerSegment)], Unit] =
    _.evalMap { chunk =>
      val (profiles, segments) = chunk.toList.unzip
      profileRepo.upsertBatch(profiles) *> segmentRepo.upsertBatch(segments) *> lastUpdateRef.set(System.currentTimeMillis())
    }

  def getCustomerInfo(customerId: Long): IO[Option[CustomerResponse]] =
    (profileRepo.get(customerId), segmentRepo.get(customerId)).mapN { (profileOpt, segmentOpt) =>
      (profileOpt, segmentOpt).mapN { (profile, segment) =>
        CustomerResponse(
          customerId = customerId,
          segment = segment.segmentName,
          confidence = segment.confidence,
          topSegments = segment.memberships.toList.sortBy(-_._2).take(3),
          lifetimeValue = profile.totalSpend,
          daysSinceLastTransaction = profile.lastTransactionDays,
          transactionCount = profile.transactionCount,
        )
      }
    }

  def getSegments: IO[SegmentsResponse] =
    for
      profiles <- profileRepo.getAll
      stats <- segmentRepo.getStats(profiles.map(p => (p.customerId, p.totalSpend)))
      count <- profileRepo.count
      lastUpdate <- lastUpdateRef.get
    yield SegmentsResponse(stats, count, lastUpdate)

  def getHealth: IO[HealthStatus] =
    profileRepo.count.map(_ => HealthStatus("ok", FeatureExtractionService.NumClusters))

object SegmentationService:
  def create(
    profileRepo: CustomerProfileRepository,
    segmentRepo: CustomerSegmentRepository,
    centroidsRepo: CentroidsRepository,
    runtime: VkCyfraRuntime,
  ): IO[SegmentationService] =
    for
      queue <- Queue.unbounded[IO, Transaction]
      lastUpdate <- Ref.of[IO, Long](System.currentTimeMillis())
      txCache <- Ref.of[IO, Map[Long, List[Transaction]]](Map.empty)
      defaultCentroids <- centroidsRepo.get
      defaultLabels <- centroidsRepo.getLabels
      centroidsCache <- Ref.of[IO, (FCMCentroids, Vector[String])]((defaultCentroids, defaultLabels))
    yield new SegmentationService(profileRepo, segmentRepo, centroidsRepo, runtime, queue, lastUpdate, txCache, centroidsCache)
