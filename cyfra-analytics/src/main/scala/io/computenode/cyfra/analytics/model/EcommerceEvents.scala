package io.computenode.cyfra.analytics.model

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.core.layout.Layout

/** Parameters for K-Means clustering */
case class KMeansParams(
  numPoints: Int32,
  numClusters: Int32,
  numFeatures: Int32
) extends GStruct[KMeansParams]

object KMeansParams:
  given GStructSchema[KMeansParams] = GStructSchema.derived

/** Layout for K-Means clustering.
  *
  * Input: Points (numPoints × numFeatures)
  *        Centroids (numClusters × numFeatures)
  *
  * Output: Cluster assignments (numPoints)
  *         New centroids (numClusters × numFeatures)
  */
case class KMeansLayout(
  points: GBuffer[Float32],
  centroids: GBuffer[Float32],
  assignments: GBuffer[Int32],
  centroidSums: GBuffer[Float32],
  clusterCounts: GBuffer[Int32],
  params: GUniform[KMeansParams]
) extends Layout

/** Layout for cluster assignment step */
case class AssignLayout(
  points: GBuffer[Float32],
  centroids: GBuffer[Float32],
  assignments: GBuffer[Int32],
  params: GUniform[KMeansParams]
) extends Layout

/** Layout for accumulating points into centroid sums */
case class AccumulateLayout(
  points: GBuffer[Float32],
  assignments: GBuffer[Int32],
  centroidSums: GBuffer[Float32],
  clusterCounts: GBuffer[Int32],
  params: GUniform[KMeansParams]
) extends Layout

/** Layout for updating centroids from sums */
case class UpdateCentroidsLayout(
  centroids: GBuffer[Float32],
  centroidSums: GBuffer[Float32],
  clusterCounts: GBuffer[Int32],
  params: GUniform[KMeansParams]
) extends Layout

/** Layout for resetting centroid accumulators */
case class ResetCentroidsLayout(
  centroidSums: GBuffer[Float32],
  clusterCounts: GBuffer[Int32],
  params: GUniform[KMeansParams]
) extends Layout
