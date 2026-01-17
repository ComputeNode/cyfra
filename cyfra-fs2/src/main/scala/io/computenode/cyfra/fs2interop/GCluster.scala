package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.library.Functions.pow
import fs2.*

import scala.compiletime.uninitialized
import scala.reflect.ClassTag

/** GPU-accelerated Fuzzy C-Means clustering for fs2 streams.
  *
  * Provides soft clustering where each data point has partial membership in multiple clusters, ideal for customer segmentation with overlapping
  * behaviors.
  *
  * ```scala
  * val centroids = FCMCentroids.custom("VIP" -> Array(0.1f, 0.9f, 0.9f), "Loyal" -> Array(0.3f, 0.7f, 0.6f), "AtRisk" -> Array(0.8f, 0.2f, 0.3f))
  *
  * customerFeatures
  *   .through(GCluster.fuzzyCMeans(config, centroids))
  *   .map { case (_, memberships) => dominantSegment(memberships) }
  * ```
  */
object GCluster:

  /** FCM configuration */
  case class FCMConfig(
    numClusters: Int,
    numFeatures: Int,
    fuzziness: Float = 2.0f, // m parameter, higher = softer clusters
    numIterations: Int = 10,
    batchSize: Int = 8192, // Optimized for bulk processing
    convergenceThreshold: Float = 0.001f,
  ):
    require(fuzziness > 1.0f, "Fuzziness must be > 1.0")

  /** Custom centroid initialization */
  case class FCMCentroids(
    labels: Vector[String],
    values: Array[Float], // flat array [cluster * numFeatures + feature]
  ):
    def numClusters: Int = labels.size
    def label(clusterId: Int): String = labels.lift(clusterId).getOrElse(s"Cluster$clusterId")

  object FCMCentroids:
    /** Create centroids with named clusters */
    def custom(centroids: (String, Array[Float])*): FCMCentroids =
      val labels = centroids.map(_._1).toVector
      val values = centroids.flatMap(_._2).toArray
      FCMCentroids(labels, values)

    /** Generate random centroids from data sample */
    def random(numClusters: Int, numFeatures: Int, seed: Long = 42): FCMCentroids =
      val r = new scala.util.Random(seed)
      val values = Array.fill(numClusters * numFeatures)(r.nextFloat())
      val labels = (0 until numClusters).map(i => s"Cluster$i").toVector
      FCMCentroids(labels, values)

  /** Membership result for a single point */
  case class Membership(values: Array[Float]):
    def dominantCluster: Int = values.indices.maxBy(values)
    def dominantWeight: Float = values.max
    def weightFor(clusterId: Int): Float = values(clusterId)
    def isAmbiguous(threshold: Float = 0.4f): Boolean = values.sorted.reverse.take(2) match
      case Array(a, b) => (a - b) < threshold
      case _           => false

  // ============================================================================
  // Internal GPU types
  // ============================================================================

  private case class FCMParams(numPoints: Int32, numClusters: Int32, numFeatures: Int32, fuzziness: Float32) extends GStruct[FCMParams]

  private object FCMParams:
    given GStructSchema[FCMParams] = GStructSchema.derived

  private given GCodec[FCMParams, (Int, Int, Int, Float)] with
    def toByteBuffer(buf: java.nio.ByteBuffer, chunk: Array[(Int, Int, Int, Float)]): java.nio.ByteBuffer =
      buf.clear().order(java.nio.ByteOrder.nativeOrder())
      chunk.foreach { case (a, b, c, d) => buf.putInt(a); buf.putInt(b); buf.putInt(c); buf.putFloat(d) }
      buf.flip(); buf
    def fromByteBuffer(buf: java.nio.ByteBuffer, arr: Array[(Int, Int, Int, Float)]): Array[(Int, Int, Int, Float)] =
      arr.indices.foreach(i => arr(i) = (buf.getInt(), buf.getInt(), buf.getInt(), buf.getFloat()))
      buf.rewind(); arr

  private case class FCMLayout(points: GBuffer[Float32], centroids: GBuffer[Float32], memberships: GBuffer[Float32], params: GUniform[FCMParams])
      derives Layout

  // ============================================================================
  // GPU Program - Membership Calculation
  // ============================================================================

  /** Computes membership matrix on GPU.
    *
    * For each point i and cluster j: u_ij = 1 / Σ_k((d_ij / d_ik)^(2/(m-1)))
    *
    * When a point is exactly on a centroid (d_ij = 0), it gets membership 1.0 for that cluster.
    */
  private def membershipProgram(config: FCMConfig): GProgram[Int, FCMLayout] =
    val exponent = 2.0f / (config.fuzziness - 1.0f)

    GProgram.static[Int, FCMLayout](
      layout = _ =>
        FCMLayout(
          points = GBuffer[Float32](config.batchSize * config.numFeatures),
          centroids = GBuffer[Float32](config.numClusters * config.numFeatures),
          memberships = GBuffer[Float32](config.batchSize * config.numClusters),
          params = GUniform[FCMParams](),
        ),
      dispatchSize = identity,
    ): layout =>
      val pointId = GIO.invocationId
      val params = layout.params.read

      GIO.when(pointId < params.numPoints):
        val pointBase = pointId * params.numFeatures

        // Compute memberships for all clusters using GIO.repeat for write operations
        GIO.repeat(params.numClusters) { j =>
          val centroidBaseJ = j * params.numFeatures

          // Distance from point to centroid j
          val distJ = GSeq
            .gen[Int32](0, _ + 1)
            .limit(config.numFeatures)
            .fold(
              0.0f,
              (sum: Float32, f: Int32) => {
                val diff = GIO.read(layout.points, pointBase + f) - GIO.read(layout.centroids, centroidBaseJ + f)
                sum + diff * diff
              },
            )

          // Handle case when point is exactly on centroid
          val membership = when(distJ < 0.000001f)(1.0f).otherwise {
            // Sum over all clusters: (d_ij / d_ik)^exponent
            val sumRatios = GSeq
              .gen[Int32](0, _ + 1)
              .limit(config.numClusters)
              .fold(
                0.0f,
                (acc: Float32, k: Int32) => {
                  val centroidBaseK = k * params.numFeatures
                  val distK = GSeq
                    .gen[Int32](0, _ + 1)
                    .limit(config.numFeatures)
                    .fold(
                      0.0f,
                      (s: Float32, f: Int32) => {
                        val d = GIO.read(layout.points, pointBase + f) - GIO.read(layout.centroids, centroidBaseK + f)
                        s + d * d
                      },
                    )
                  // Avoid division by zero
                  val ratio = when(distK < 0.000001f)(0.0f).otherwise(pow(distJ / distK, exponent))
                  acc + ratio
                },
              )
            when(sumRatios > 0.0f)(1.0f / sumRatios).otherwise(1.0f / params.numClusters.asFloat)
          }

          GIO.write(layout.memberships, pointId * params.numClusters + j, membership)
        }

  // ============================================================================
  // FCM State Management
  // ============================================================================

  private class FCMState(config: FCMConfig, initialCentroids: FCMCentroids):
    private var centroids: Array[Float] = initialCentroids.values.clone()
    private var initialized = initialCentroids.values.nonEmpty
    private val program = membershipProgram(config)
    private val membershipResults = new Array[Float](config.batchSize * config.numClusters)

    def getCentroids: Array[Float] = centroids

    def initializeFromData(batch: Array[Array[Float]]): Unit =
      if !initialized && batch.length >= config.numClusters then
        val random = new scala.util.Random(42)
        val indices = random.shuffle(batch.indices.toList).take(config.numClusters)
        centroids = new Array[Float](config.numClusters * config.numFeatures)
        indices.zipWithIndex.foreach { case (pi, ci) =>
          System.arraycopy(batch(pi), 0, centroids, ci * config.numFeatures, config.numFeatures)
        }
        initialized = true

    def computeMemberships(batch: Array[Array[Float]])(using CyfraRuntime): Array[Membership] =
      val n = batch.length
      val flat = new Array[Float](config.batchSize * config.numFeatures)
      batch.indices.foreach(i => System.arraycopy(batch(i), 0, flat, i * config.numFeatures, config.numFeatures))

      GBufferRegion
        .allocate[FCMLayout]
        .map { layout =>
          program.execute(n, layout); layout
        }
        .runUnsafe(
          init = FCMLayout(
            points = GBuffer[Float, Float32](flat),
            centroids = GBuffer[Float, Float32](centroids),
            memberships = GBuffer[Float32](config.batchSize * config.numClusters),
            params = GUniform[(Int, Int, Int, Float), FCMParams]((n, config.numClusters, config.numFeatures, config.fuzziness)),
          ),
          onDone = _.memberships.readArray[Float](membershipResults),
        )

      (0 until n).map { i =>
        val m = new Array[Float](config.numClusters)
        System.arraycopy(membershipResults, i * config.numClusters, m, 0, config.numClusters)
        Membership(m)
      }.toArray

    /** Update centroids using fuzzy weighted average: c_j = Σ(u_ij^m * x_i) / Σ(u_ij^m)
      */
    def updateCentroids(batch: Array[Array[Float]], memberships: Array[Membership]): Unit =
      val newCentroids = new Array[Float](config.numClusters * config.numFeatures)
      val weights = new Array[Float](config.numClusters)

      batch.indices.foreach { i =>
        val point = batch(i)
        val u = memberships(i).values

        var j = 0;
        while j < config.numClusters do
          val uPowM = math.pow(u(j), config.fuzziness).toFloat
          weights(j) += uPowM

          var f = 0;
          while f < config.numFeatures do
            newCentroids(j * config.numFeatures + f) += uPowM * point(f)
            f += 1
          j += 1
      }

      // Normalize by weights
      var j = 0;
      while j < config.numClusters do
        if weights(j) > 0.0001f then
          var f = 0;
          while f < config.numFeatures do
            val idx = j * config.numFeatures + f
            newCentroids(idx) /= weights(j)
            f += 1
        j += 1

      centroids = newCentroids

  // ============================================================================
  // Public API
  // ============================================================================

  /** Fuzzy C-Means clustering as an fs2 Pipe.
    *
    * Returns membership vectors showing degree of belonging to each cluster. Uses GPU for parallel membership computation, CPU for centroid updates.
    */
  def fuzzyCMeans[F[_]](config: FCMConfig, centroids: FCMCentroids)(using CyfraRuntime): Pipe[F, Array[Float], (Array[Float], Membership)] =
    val state = new FCMState(config, centroids)

    _.chunkN(config.batchSize).flatMap { chunk =>
      val batch = chunk.toArray

      // Initialize from data if no custom centroids
      state.initializeFromData(batch)

      // Initial iterations on first batch
      if batch.indices.head == 0 then
        (0 until config.numIterations).foreach { _ =>
          val m = state.computeMemberships(batch)
          state.updateCentroids(batch, m)
        }

      // Compute final memberships + incremental update
      val memberships = state.computeMemberships(batch)
      state.updateCentroids(batch, memberships)

      Stream.emits(batch.zip(memberships))
    }

  /** FCM returning dominant cluster assignment (hard clustering from soft) */
  def fuzzyCMeansHard[F[_]](config: FCMConfig, centroids: FCMCentroids)(using CyfraRuntime): Pipe[F, Array[Float], Int] =
    fuzzyCMeans[F](config, centroids).andThen(_.map(_._2.dominantCluster))

  /** FCM with custom type conversion */
  def fuzzyCMeansTyped[F[_], A: ClassTag, B](
    config: FCMConfig,
    centroids: FCMCentroids,
    toFeatures: A => Array[Float],
    withMembership: (A, Membership) => B,
  )(using CyfraRuntime): Pipe[F, A, B] =
    _.map(a => (a, toFeatures(a)))
      .through(fuzzyCMeansWithInput[F, A](config, centroids))
      .map { case (a, m) => withMembership(a, m) }

  /** FCM preserving original input */
  def fuzzyCMeansWithInput[F[_], A: ClassTag](config: FCMConfig, centroids: FCMCentroids)(using
    CyfraRuntime,
  ): Pipe[F, (A, Array[Float]), (A, Membership)] =
    val state = new FCMState(config, centroids)

    _.chunkN(config.batchSize).flatMap { chunk =>
      val batch = chunk.toArray
      val features = batch.map(_._2)

      state.initializeFromData(features)

      if batch.indices.head == 0 then
        (0 until config.numIterations).foreach { _ =>
          val m = state.computeMemberships(features)
          state.updateCentroids(features, m)
        }

      val memberships = state.computeMemberships(features)
      state.updateCentroids(features, memberships)

      Stream.emits(batch.map(_._1).zip(memberships))
    }
