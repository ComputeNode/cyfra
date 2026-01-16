package io.computenode.cyfra.analytics.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

/** Raw transaction event */
case class Transaction(
  customerId: Long,
  timestamp: Long,
  amount: Double,
  items: Int,
  category: Int,
  channel: String,
  discountPct: Double
)

object Transaction:
  given Decoder[Transaction] = deriveDecoder
  given Encoder[Transaction] = deriveEncoder

/** Customer behavioral profile with 50 features */
case class CustomerProfile(
  customerId: Long,
  features: Array[Float],
  transactionCount: Int,
  lastUpdate: Long,
  totalSpend: Double,
  lastTransactionDays: Int
)

/** Segment assignment with fuzzy membership */
case class CustomerSegment(
  customerId: Long,
  segmentName: String,
  confidence: Float,
  memberships: Map[String, Float],
  assignedAt: Long
)

object CustomerSegment:
  given Encoder[CustomerSegment] = deriveEncoder
  given Decoder[CustomerSegment] = deriveDecoder

/** Segment statistics */
case class SegmentInfo(
  name: String,
  customerCount: Int,
  avgLifetimeValue: Double,
  avgRecency: Double
)

object SegmentInfo:
  given Encoder[SegmentInfo] = deriveEncoder
  given Decoder[SegmentInfo] = deriveDecoder

/** API response models */
case class CustomerResponse(
  customerId: Long,
  segment: String,
  confidence: Float,
  topSegments: List[(String, Float)],
  lifetimeValue: Double,
  daysSinceLastTransaction: Int,
  transactionCount: Int
)

object CustomerResponse:
  given Encoder[CustomerResponse] = deriveEncoder
  given Decoder[CustomerResponse] = deriveDecoder

case class SegmentsResponse(
  segments: List[SegmentInfo],
  totalCustomers: Int,
  lastUpdated: Long
)

object SegmentsResponse:
  given Encoder[SegmentsResponse] = deriveEncoder
  given Decoder[SegmentsResponse] = deriveDecoder

case class HealthStatus(status: String, clustersActive: Int)

object HealthStatus:
  given Encoder[HealthStatus] = deriveEncoder
  given Decoder[HealthStatus] = deriveDecoder

case class ErrorResponse(error: String)

object ErrorResponse:
  given Encoder[ErrorResponse] = deriveEncoder
  given Decoder[ErrorResponse] = deriveDecoder

case class BatchResponse(accepted: Int, message: String)

object BatchResponse:
  given Encoder[BatchResponse] = deriveEncoder
  given Decoder[BatchResponse] = deriveDecoder
