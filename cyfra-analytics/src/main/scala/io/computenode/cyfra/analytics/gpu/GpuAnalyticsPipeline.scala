package io.computenode.cyfra.analytics.gpu

import fs2.{Pipe, Stream}
import io.computenode.cyfra.core.{CyfraRuntime, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.dsl.library.Functions.pow
import io.computenode.cyfra.fs2interop.GCluster.{FCMCentroids, Membership}
import io.computenode.cyfra.fs2interop.GPipe
import io.computenode.cyfra.analytics.model.CustomerProfile

/** GPU-accelerated customer segmentation pipeline. */
object GpuAnalyticsPipeline:

  case class Config(numFeatures: Int, numClusters: Int, fuzziness: Float = 2.0f, batchSize: Int = 100_000)

  case class SegmentResult(customer: CustomerProfile, membership: Membership, dominantSegment: Int):
    def dominantWeight: Float = membership.dominantWeight

  def segment[F[_]](config: Config, centroids: FCMCentroids)(using CyfraRuntime): Pipe[F, CustomerProfile, SegmentResult] =
    _.chunkN(config.batchSize).flatMap { chunk =>
      val customers = chunk.toArray

      val memberships = GPipe.batch(membershipKernel(config), config.batchSize, config.numFeatures, config.numClusters)(
        buildLayout = (flat, n) =>
          FCMLayout(
            points = GBuffer[Float, Float32](flat),
            centroids = GBuffer[Float, Float32](centroids.values),
            memberships = GBuffer[Float32](config.batchSize * config.numClusters),
            params = GUniform[(Int, Int, Int, Float), FCMParams]((n, config.numFeatures, config.numClusters, config.fuzziness)),
          ),
        outputBuffer = _.memberships,
      )(customers.map(_.features))

      Stream.emits(customers.zip(memberships).map { case (customer, m) =>
        val membership = Membership(m)
        SegmentResult(customer, membership, membership.dominantCluster)
      })
    }

  // GPU Kernel

  private case class FCMParams(numPoints: Int32, numFeatures: Int32, numClusters: Int32, fuzziness: Float32) extends GStruct[FCMParams]

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

  private def membershipKernel(config: Config): GProgram[Int, FCMLayout] =
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
      val exponent = 2.0f / (config.fuzziness - 1.0f)

      GIO.when(pointId < params.numPoints):
        val pointBase = pointId * params.numFeatures

        GIO.repeat(params.numClusters) { j =>
          val centroidBaseJ = j * params.numFeatures

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

          val membership = when(distJ < 0.000001f)(1.0f).otherwise {
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
                  val ratio = when(distK < 0.000001f)(0.0f).otherwise(pow(distJ / distK, exponent))
                  acc + ratio
                },
              )
            when(sumRatios > 0.0f)(1.0f / sumRatios).otherwise(1.0f / params.numClusters.asFloat)
          }

          GIO.write(layout.memberships, pointId * params.numClusters + j, membership)
        }
