package io.computenode.cyfra.analytics.repository

import cats.effect.{IO, Ref}
import io.computenode.cyfra.fs2interop.GCluster.FCMCentroids
import io.computenode.cyfra.analytics.service.FeatureExtractionService

/** In-memory storage for cluster centroids. */
class InMemoryCentroidsRepository(centroidsRef: Ref[IO, FCMCentroids]) extends CentroidsRepository:

  def get: IO[FCMCentroids] = centroidsRef.get

  def update(centroids: FCMCentroids): IO[Unit] = centroidsRef.set(centroids)

  def getLabels: IO[Vector[String]] = centroidsRef.get.map(_.labels)

object InMemoryCentroidsRepository:

  val SegmentLabels: Vector[String] = Vector(
    "Elite VIP",
    "Premium Loyalist",
    "High-Value Regular",
    "Luxury Seeker",
    "Premium Explorer",
    "Brand Champion",
    "Loyal Regular",
    "Steady Shopper",
    "Consistent Buyer",
    "Reliable Customer",
    "Rising Star",
    "Growth Potential",
    "Accelerating Buyer",
    "Emerging VIP",
    "Upward Trend",
    "Active Enthusiast",
    "Frequent Browser",
    "Omnichannel Power User",
    "Mobile Native",
    "Web Regular",
    "Smart Saver",
    "Deal Hunter",
    "Discount Maximizer",
    "Budget Conscious",
    "Price Sensitive",
    "Holiday Shopper",
    "Weekend Warrior",
    "Evening Buyer",
    "Morning Shopper",
    "Lunch Break Browser",
    "New VIP Potential",
    "Promising Newcomer",
    "First-Time Buyer",
    "Curious Explorer",
    "Trial Customer",
    "Fading VIP",
    "Declining Loyal",
    "Dormant Regular",
    "Churning Risk",
    "Lapsing Customer",
    "Category Specialist",
    "Single-Brand Loyalist",
    "Bulk Buyer",
    "Gift Giver",
    "Subscription Seeker",
    "Impulse Buyer",
    "Planned Purchaser",
    "Research-Heavy",
    "Quick Converter",
    "Browse-Heavy",
  )

  def generateDefaultCentroids: FCMCentroids =
    val numClusters = FeatureExtractionService.NumClusters
    val numFeatures = FeatureExtractionService.NumFeatures
    val random = new scala.util.Random(42)

    val values = new Array[Float](numClusters * numFeatures)

    (0 until numClusters).foreach { cluster =>
      val baseOffset = cluster * numFeatures
      val archetype = cluster / 5
      val variation = cluster % 5

      (0 until numFeatures).foreach { f =>
        values(baseOffset + f) = 0.3f + random.nextFloat() * 0.4f
      }

      archetype match
        case 0 =>
          values(baseOffset + 0) = 0.1f + variation * 0.05f
          values(baseOffset + 2) = 0.85f + variation * 0.03f
          values(baseOffset + 3) = 0.8f + variation * 0.04f
        case 1 =>
          values(baseOffset + 1) = 0.75f + variation * 0.05f
          values(baseOffset + 6) = 0.3f + variation * 0.1f
          values(baseOffset + 8) = 0.6f + variation * 0.08f
        case 2 =>
          values(baseOffset + 73) = 0.7f + variation * 0.06f
          values(baseOffset + 74) = 0.65f + variation * 0.07f
        case 3 =>
          values(baseOffset + 9) = 0.6f + variation * 0.08f
          values(baseOffset + 27) = 0.75f + variation * 0.05f
          values(baseOffset + 28) = 0.8f + variation * 0.04f
        case 4 =>
          values(baseOffset + 7) = 0.7f + variation * 0.06f
          values(baseOffset + 45) = 0.8f + variation * 0.04f
          values(baseOffset + 49) = 0.75f + variation * 0.05f
        case 5 =>
          values(baseOffset + 10 + variation) = 0.7f
          values(baseOffset + 16 + variation) = 0.65f
        case 6 =>
          values(baseOffset + 65) = 0.15f + variation * 0.05f
          values(baseOffset + 0) = 0.2f + variation * 0.1f
          values(baseOffset + 1) = 0.25f + variation * 0.1f
        case 7 =>
          values(baseOffset + 0) = 0.7f + variation * 0.06f
          values(baseOffset + 73) = 0.2f - variation * 0.04f
          values(baseOffset + 77) = 0.15f - variation * 0.03f
        case 8 =>
          values(baseOffset + 8) = 0.2f + variation * 0.1f
          values(baseOffset + 97) = 0.8f + variation * 0.04f
        case 9 =>
          values(baseOffset + 55) = 0.5f + variation * 0.1f
          values(baseOffset + 98) = 0.6f + variation * 0.08f
    }

    FCMCentroids(SegmentLabels, values)

  def create: IO[InMemoryCentroidsRepository] =
    Ref.of[IO, FCMCentroids](generateDefaultCentroids).map(new InMemoryCentroidsRepository(_))
