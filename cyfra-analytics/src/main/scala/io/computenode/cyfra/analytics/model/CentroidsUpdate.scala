package io.computenode.cyfra.analytics.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

/** Request to update segment centroids */
case class CentroidsUpdate(labels: Vector[String], values: Array[Float])

object CentroidsUpdate:
  given Decoder[CentroidsUpdate] = deriveDecoder
  given Encoder[CentroidsUpdate] = deriveEncoder

/** Response with current centroids */
case class CentroidsResponse(labels: Vector[String], numClusters: Int, numFeatures: Int)

object CentroidsResponse:
  given Encoder[CentroidsResponse] = deriveEncoder
  given Decoder[CentroidsResponse] = deriveDecoder
