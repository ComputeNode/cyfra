package io.computenode.cyfra.fs2interop

import io.computenode.cyfra.core.{CyfraRuntime, GBufferRegion, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import fs2.*

import scala.compiletime.uninitialized
import scala.reflect.ClassTag

/** GPU-accelerated K-Means clustering for fs2 streams.
  *
  * Seamlessly integrates GPU clustering into functional streaming pipelines:
  *
  * ```scala
  * customerFeatures
  *   .through(GCluster.kMeans(config))
  *   .map { case (features, clusterId) => assignSegment(clusterId) }
  * ```
  */
object GCluster:

  /** K-Means configuration */
  case class KMeansConfig(
    numClusters: Int,
    numFeatures: Int,
    numIterations: Int = 10,
    batchSize: Int = 1024,
    seed: Long = 42
  )

  // ============================================================================
  // Internal GPU types
  // ============================================================================

  private case class AssignParams(
    numPoints: Int32,
    numClusters: Int32,
    numFeatures: Int32
  ) extends GStruct[AssignParams]

  private object AssignParams:
    given GStructSchema[AssignParams] = GStructSchema.derived

  private given GCodec[AssignParams, (Int, Int, Int)] with
    def toByteBuffer(buf: java.nio.ByteBuffer, chunk: Array[(Int, Int, Int)]): java.nio.ByteBuffer =
      buf.clear().order(java.nio.ByteOrder.nativeOrder())
      chunk.foreach { case (a, b, c) => buf.putInt(a); buf.putInt(b); buf.putInt(c) }
      buf.flip(); buf
    def fromByteBuffer(buf: java.nio.ByteBuffer, arr: Array[(Int, Int, Int)]): Array[(Int, Int, Int)] =
      arr.indices.foreach(i => arr(i) = (buf.getInt(), buf.getInt(), buf.getInt()))
      buf.rewind(); arr

  private case class AssignLayout(
    points: GBuffer[Float32],
    centroids: GBuffer[Float32],
    assignments: GBuffer[Int32],
    params: GUniform[AssignParams]
  ) extends Layout

  private case class NearestAcc(clusterId: Int32, minDist: Float32) extends GStruct[NearestAcc]
  private object NearestAcc:
    given GStructSchema[NearestAcc] = GStructSchema.derived

  // ============================================================================
  // GPU Program
  // ============================================================================

  private def assignProgram(config: KMeansConfig): GProgram[Int, AssignLayout] =
    GProgram.static[Int, AssignLayout](
      layout = _ => AssignLayout(
        points = GBuffer[Float32](config.batchSize * config.numFeatures),
        centroids = GBuffer[Float32](config.numClusters * config.numFeatures),
        assignments = GBuffer[Int32](config.batchSize),
        params = GUniform[AssignParams]()
      ),
      dispatchSize = identity  // numPoints passed directly
    ): layout =>
      val pointId = GIO.invocationId
      val params = layout.params.read

      GIO.when(pointId < params.numPoints):
        val pointBase = pointId * params.numFeatures

        val nearest = GSeq.gen[Int32](0, _ + 1).limit(config.numClusters)
          .fold(NearestAcc(0, Float.MaxValue), (acc: NearestAcc, clusterId: Int32) => {
            val centroidBase = clusterId * params.numFeatures
            val distSq = GSeq.gen[Int32](0, _ + 1).limit(config.numFeatures)
              .fold(0.0f, (sum: Float32, f: Int32) => {
                val diff = GIO.read(layout.points, pointBase + f) - GIO.read(layout.centroids, centroidBase + f)
                sum + diff * diff
              })
            when(distSq < acc.minDist)(NearestAcc(clusterId, distSq)).otherwise(acc)
          })

        GIO.write(layout.assignments, pointId, nearest.clusterId)

  // ============================================================================
  // Streaming K-Means State
  // ============================================================================

  private class KMeansState(config: KMeansConfig):
    private val random = new scala.util.Random(config.seed)
    private var centroids: Array[Float] = uninitialized
    private var counts: Array[Int] = uninitialized
    private var initialized = false
    private val program = assignProgram(config)
    private val results = new Array[Int](config.batchSize)

    def isInitialized: Boolean = initialized

    def initialize(batch: Array[Array[Float]]): Unit =
      val indices = random.shuffle(batch.indices.toList).take(config.numClusters)
      centroids = new Array[Float](config.numClusters * config.numFeatures)
      indices.zipWithIndex.foreach { case (pi, ci) =>
        System.arraycopy(batch(pi), 0, centroids, ci * config.numFeatures, config.numFeatures)
      }
      counts = Array.fill(config.numClusters)(1)
      initialized = true

    def assign(batch: Array[Array[Float]])(using cr: CyfraRuntime): Array[Int] =
      val n = batch.length
      val flat = new Array[Float](config.batchSize * config.numFeatures)
      batch.indices.foreach(i => System.arraycopy(batch(i), 0, flat, i * config.numFeatures, config.numFeatures))

      GBufferRegion.allocate[AssignLayout]
        .map(layout => { program.execute(n, layout); layout })
        .runUnsafe(
          init = AssignLayout(
            points = GBuffer[Float, Float32](flat),
            centroids = GBuffer[Float, Float32](centroids),
            assignments = GBuffer[Int32](config.batchSize),
            params = GUniform[(Int, Int, Int), AssignParams]((n, config.numClusters, config.numFeatures))
          ),
          onDone = _.assignments.readArray[Int](results)
        )

      results.take(n)

    def updateCentroids(batch: Array[Array[Float]], assignments: Array[Int]): Unit =
      val sums = new Array[Float](config.numClusters * config.numFeatures)
      val batchCounts = new Array[Int](config.numClusters)

      batch.indices.foreach { i =>
        val c = assignments(i)
        batchCounts(c) += 1
        var f = 0; while f < config.numFeatures do
          sums(c * config.numFeatures + f) += batch(i)(f)
          f += 1
      }

      var c = 0; while c < config.numClusters do
        if batchCounts(c) > 0 then
          val oldW = counts(c).toFloat
          val newW = batchCounts(c).toFloat
          val total = oldW + newW
          var f = 0; while f < config.numFeatures do
            val idx = c * config.numFeatures + f
            centroids(idx) = (centroids(idx) * oldW + sums(idx)) / total
            f += 1
          counts(c) += batchCounts(c)
        c += 1

  // ============================================================================
  // Public API
  // ============================================================================

  /** Mini-batch K-Means clustering as an fs2 Pipe.
    *
    * Processes points in batches, using GPU for nearest-centroid assignment
    * and CPU for incremental centroid updates (mini-batch K-Means).
    */
  def kMeans[F[_]](config: KMeansConfig)(using cr: CyfraRuntime): Pipe[F, Array[Float], (Array[Float], Int)] =
    val state = new KMeansState(config)

    _.chunkN(config.batchSize).flatMap { chunk =>
      val batch = chunk.toArray

      // Initialize centroids from first batch
      if !state.isInitialized then
        state.initialize(batch)
        (0 until config.numIterations).foreach { _ =>
          val a = state.assign(batch)
          state.updateCentroids(batch, a)
        }

      // GPU assign + CPU centroid update
      val assignments = state.assign(batch)
      state.updateCentroids(batch, assignments)

      Stream.emits(batch.zip(assignments))
    }

  /** K-Means returning only cluster IDs */
  def kMeansIds[F[_]](config: KMeansConfig)(using CyfraRuntime): Pipe[F, Array[Float], Int] =
    kMeans[F](config).andThen(_.map(_._2))

  /** K-Means with custom type conversion */
  def kMeansTyped[F[_], A: ClassTag, B](
    config: KMeansConfig,
    toFeatures: A => Array[Float],
    withCluster: (A, Int) => B
  )(using CyfraRuntime): Pipe[F, A, B] =
    _.map(a => (a, toFeatures(a)))
      .through(kMeansWithInput[F, A](config))
      .map { case (a, c) => withCluster(a, c) }

  /** K-Means preserving original input */
  def kMeansWithInput[F[_], A: ClassTag](config: KMeansConfig)(using cr: CyfraRuntime): Pipe[F, (A, Array[Float]), (A, Int)] =
    val state = new KMeansState(config)

    _.chunkN(config.batchSize).flatMap { chunk =>
      val batch = chunk.toArray
      val features = batch.map(_._2)

      if !state.isInitialized then
        state.initialize(features)
        (0 until config.numIterations).foreach { _ =>
          val a = state.assign(features)
          state.updateCentroids(features, a)
        }

      val assignments = state.assign(features)
      state.updateCentroids(features, assignments)

      Stream.emits(batch.map(_._1).zip(assignments))
    }
