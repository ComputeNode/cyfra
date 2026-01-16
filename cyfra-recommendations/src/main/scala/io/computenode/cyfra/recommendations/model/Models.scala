package io.computenode.cyfra.recommendations.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

/** Product with precomputed embedding for similarity search */
case class Product(
  id: Long,
  name: String,
  category: String,
  price: Double,
  embedding: List[Float]  // 128-dimensional vector
) derives Encoder.AsObject, Decoder

/** Request for similar product recommendations */
case class RecommendationRequest(
  productId: Long,
  limit: Option[Int] = Some(10),
  minScore: Option[Float] = None
) derives Encoder.AsObject, Decoder

/** A single product recommendation with similarity score */
case class SimilarProduct(
  product: Product,
  similarityScore: Float
) derives Encoder.AsObject, Decoder

/** Response containing similar products */
case class RecommendationResponse(
  requestedProductId: Long,
  recommendations: List[SimilarProduct],
  computeTimeMs: Long
) derives Encoder.AsObject, Decoder

/** Health check response */
case class HealthStatus(
  status: String,
  gpuAvailable: Boolean,
  productsLoaded: Int,
  embeddingDimension: Int
) derives Encoder.AsObject, Decoder

/** Error response */
case class ErrorResponse(
  error: String,
  message: String
) derives Encoder.AsObject, Decoder
