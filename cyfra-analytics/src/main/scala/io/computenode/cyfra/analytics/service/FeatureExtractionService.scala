package io.computenode.cyfra.analytics.service

import io.computenode.cyfra.analytics.model.Transaction

/** Extracts 100 behavioral features from transaction history.
  *
  * Feature groups:
  *  - RFM Core (10): recency, frequency, monetary, avg order, etc.
  *  - Temporal (15): hourly patterns, day of week, seasonal
  *  - Product (20): category preferences, diversity, trends
  *  - Engagement (15): session patterns, channel mix
  *  - Value (15): discount sensitivity, price tier
  *  - Lifecycle (15): tenure, growth rate, churn risk
  *  - Behavioral (10): browsing patterns, return rate
  */
object FeatureExtractionService:

  val NumFeatures = 100
  val NumClusters = 50

  def computeFeatures(transactions: List[Transaction]): Array[Float] =
    if transactions.isEmpty then Array.fill(NumFeatures)(0.5f)
    else
      val ctx = FeatureContext(transactions)
      Array(
        // RFM Core (10)
        norm(ctx.recencyDays, 365f),
        norm(ctx.frequency, 10f),
        norm(ctx.totalSpend, 50000f),
        norm(ctx.avgOrderValue, 500f),
        norm(ctx.amountStdDev, 200f),
        norm(ctx.avgItems, 10f),
        norm(ctx.avgGapDays, 90f),
        norm(ctx.avgDiscount, 0.5f),
        norm(ctx.categoryDiversity, 1f),
        norm(ctx.mobileRatio, 1f),
        
        // Temporal - Hourly (6)
        norm(ctx.hourRatio(0, 4), 1f),
        norm(ctx.hourRatio(4, 8), 1f),
        norm(ctx.hourRatio(8, 12), 1f),
        norm(ctx.hourRatio(12, 16), 1f),
        norm(ctx.hourRatio(16, 20), 1f),
        norm(ctx.hourRatio(20, 24), 1f),
        
        // Temporal - Day of week (7)
        norm(ctx.dayOfWeekRatio(0), 1f),
        norm(ctx.dayOfWeekRatio(1), 1f),
        norm(ctx.dayOfWeekRatio(2), 1f),
        norm(ctx.dayOfWeekRatio(3), 1f),
        norm(ctx.dayOfWeekRatio(4), 1f),
        norm(ctx.dayOfWeekRatio(5), 1f),
        norm(ctx.dayOfWeekRatio(6), 1f),
        
        // Temporal - Gaps (2)
        norm(ctx.minGap, 30f),
        norm(ctx.maxGap, 180f),
        
        // Product - Category spend (10)
        norm(ctx.categorySpend(0), 5000f),
        norm(ctx.categorySpend(1), 5000f),
        norm(ctx.categorySpend(2), 5000f),
        norm(ctx.categorySpend(3), 5000f),
        norm(ctx.categorySpend(4), 5000f),
        norm(ctx.categorySpend(5), 5000f),
        norm(ctx.categorySpend(6), 5000f),
        norm(ctx.categorySpend(7), 5000f),
        norm(ctx.categorySpend(8), 5000f),
        norm(ctx.categorySpend(9), 5000f),
        
        // Product - Category frequency (10)
        norm(ctx.categoryFreq(0), 1f),
        norm(ctx.categoryFreq(1), 1f),
        norm(ctx.categoryFreq(2), 1f),
        norm(ctx.categoryFreq(3), 1f),
        norm(ctx.categoryFreq(4), 1f),
        norm(ctx.categoryFreq(5), 1f),
        norm(ctx.categoryFreq(6), 1f),
        norm(ctx.categoryFreq(7), 1f),
        norm(ctx.categoryFreq(8), 1f),
        norm(ctx.categoryFreq(9), 1f),
        
        // Engagement (10)
        norm(ctx.webRatio, 1f),
        norm(ctx.mobileRatio, 1f),
        norm(ctx.multiChannel, 1f),
        norm(ctx.txCount, 100f),
        norm(ctx.purchaseVelocity, 1f),
        norm(ctx.uniqueDays, 180f),
        norm(ctx.txPerDay, 5f),
        norm(ctx.maxAmount, 1000f),
        norm(ctx.minAmount, 500f),
        norm(ctx.amountRange, 500f),
        
        // Value (15)
        norm(ctx.maxItems, 20f),
        norm(ctx.minItems, 10f),
        norm(ctx.totalItems, 500f),
        norm(ctx.top3Concentration, 1f),
        norm(ctx.maxDiscount, 0.5f),
        norm(ctx.minDiscount, 0.5f),
        norm(ctx.discountRange, 0.5f),
        norm(ctx.discountStdDev, 0.2f),
        norm(ctx.highDiscountRatio, 1f),
        norm(ctx.highValueRatio, 1f),
        norm(ctx.lowValueRatio, 1f),
        norm(ctx.midValueRatio, 1f),
        norm(ctx.singleItemRatio, 1f),
        norm(ctx.bulkPurchaseRatio, 1f),
        norm(ctx.avgPricePerItem, 100f),
        
        // Lifecycle (15)
        norm(ctx.itemVariety, 1f),
        norm(ctx.medianAmount, 500f),
        norm(ctx.q3Amount, 500f),
        norm(ctx.q1Amount, 500f),
        norm(ctx.tenure, 365f),
        norm(ctx.monthlyTxRate, 10f),
        norm(ctx.monthlySpendRate, 2000f),
        norm(ctx.spendGrowth, 200f),
        norm(ctx.spendGrowthPct, 2f),
        norm(ctx.frequencyGrowth, 20f),
        norm(ctx.recentTxCount, 10f),
        norm(ctx.recentSpend, 2000f),
        norm(ctx.isInactive, 1f),
        norm(ctx.tenureNorm, 1f),
        norm(ctx.activityNorm, 1f),
        
        // Behavioral (15)
        norm(ctx.spendNorm, 1f),
        norm(ctx.avgMonthlyTx, 5f),
        norm(ctx.categoryBreadth, 1f),
        norm(ctx.daysCoverage, 1f),
        norm(ctx.estimatedViews, 500f),
        norm(ctx.estimatedReturns, 10f),
        norm(ctx.estimatedCartAbandonment, 5f),
        norm(ctx.estimatedWishlist, 30f),
        norm(ctx.estimatedReviews, 150f),
        norm(ctx.conversionRate, 1f),
        norm(ctx.priceSensitivity, 1f),
        norm(ctx.categoryConcentration, 1f),
        norm(ctx.purchaseRegularity, 1f),
        norm(ctx.spendSkew, 200f),
        norm(ctx.recencyScore, 1f)
      )

  private case class FeatureContext(transactions: List[Transaction]):
    private val now = System.currentTimeMillis()
    private val day = 86400000.0
    
    val amounts: List[Double] = transactions.map(_.amount)
    val timestamps: List[Long] = transactions.map(_.timestamp)
    val gaps: List[Double] = timestamps.sorted.sliding(2).collect { case List(a, b) => (b - a) / day }.toList
    val categories: List[Int] = transactions.map(_.category)
    val channels: List[String] = transactions.map(_.channel)
    val hours: List[Int] = timestamps.map(t => ((t / 3600000) % 24).toInt)
    val daysOfWeek: List[Int] = timestamps.map(t => ((t / 86400000) % 7).toInt)
    val discounts: List[Double] = transactions.map(_.discountPct)
    
    val txCount: Float = transactions.size.toFloat
    val totalSpend: Float = amounts.sum.toFloat
    val avgOrderValue: Float = (amounts.sum / transactions.size).toFloat
    val recencyDays: Float = ((now - timestamps.max) / day).toFloat
    val frequency: Float = (transactions.size / ((now - timestamps.min) / day / 30 + 1)).toFloat
    val amountStdDev: Float = if amounts.size > 1 then stdDev(amounts).toFloat else 0f
    val avgItems: Float = transactions.map(_.items).sum.toFloat / transactions.size
    val avgGapDays: Float = if gaps.nonEmpty then (gaps.sum / gaps.size).toFloat else 30f
    val avgDiscount: Float = (discounts.sum / transactions.size).toFloat
    val categoryDiversity: Float = categories.distinct.size.toFloat / 20
    
    def hourRatio(from: Int, to: Int): Float = hours.count(h => h >= from && h < to).toFloat / transactions.size
    def dayOfWeekRatio(d: Int): Float = daysOfWeek.count(_ == d).toFloat / transactions.size
    
    val minGap: Float = if gaps.nonEmpty then gaps.min.toFloat else 1f
    val maxGap: Float = if gaps.nonEmpty then gaps.max.toFloat else 90f
    
    private val categorySpendMap = transactions.groupBy(_.category).view.mapValues(_.map(_.amount).sum).toMap
    private val categoryFreqMap = categories.groupBy(identity).view.mapValues(_.size.toFloat).toMap
    def categorySpend(cat: Int): Float = categorySpendMap.getOrElse(cat, 0.0).toFloat
    def categoryFreq(cat: Int): Float = categoryFreqMap.getOrElse(cat, 0f) / transactions.size
    
    val webCount: Int = channels.count(_ == "web")
    val mobileCount: Int = channels.count(_ == "mobile_app")
    val webRatio: Float = webCount.toFloat / transactions.size
    val mobileRatio: Float = mobileCount.toFloat / transactions.size
    val multiChannel: Float = if webCount > 0 && mobileCount > 0 then 1f else 0f
    val purchaseVelocity: Float = if gaps.nonEmpty then 1f / ((gaps.sum / gaps.size).toFloat + 1) else 0.1f
    
    val uniqueDays: Float = timestamps.map(_ / day.toLong).distinct.size.toFloat
    val txPerDay: Float = transactions.size.toFloat / uniqueDays
    val maxAmount: Float = amounts.max.toFloat
    val minAmount: Float = amounts.min.toFloat
    val amountRange: Float = (amounts.max - amounts.min).toFloat
    val maxItems: Float = transactions.map(_.items).max.toFloat
    val minItems: Float = transactions.map(_.items).min.toFloat
    val totalItems: Float = transactions.map(_.items).sum.toFloat
    val top3Concentration: Float = amounts.sorted.reverse.take(3).sum.toFloat / amounts.sum.toFloat
    
    val maxDiscount: Float = discounts.max.toFloat
    val minDiscount: Float = discounts.min.toFloat
    val discountRange: Float = (discounts.max - discounts.min).toFloat
    val discountStdDev: Float = if discounts.size > 1 then stdDev(discounts).toFloat else 0f
    val highDiscountRatio: Float = transactions.count(_.discountPct > 0.1).toFloat / transactions.size
    
    private val avgAmount = amounts.sum / amounts.size
    val highValueRatio: Float = transactions.count(_.amount > avgAmount * 1.5).toFloat / transactions.size
    val lowValueRatio: Float = transactions.count(_.amount < avgAmount * 0.5).toFloat / transactions.size
    val midValueRatio: Float = transactions.count(t => t.amount >= avgAmount * 0.5 && t.amount <= avgAmount * 1.5).toFloat / transactions.size
    val singleItemRatio: Float = transactions.count(_.items == 1).toFloat / transactions.size
    val bulkPurchaseRatio: Float = transactions.count(_.items > 3).toFloat / transactions.size
    val avgPricePerItem: Float = (amounts.sum / transactions.map(_.items).sum).toFloat
    val itemVariety: Float = transactions.map(_.items).toSet.size.toFloat / 10
    val medianAmount: Float = amounts.sorted.apply(amounts.size / 2).toFloat
    val q3Amount: Float = if amounts.size >= 4 then amounts.sorted.apply(amounts.size * 3 / 4).toFloat else amounts.max.toFloat
    val q1Amount: Float = if amounts.size >= 4 then amounts.sorted.apply(amounts.size / 4).toFloat else amounts.min.toFloat
    
    val tenure: Float = ((now - timestamps.min) / day).toFloat
    val monthlyTxRate: Float = transactions.size.toFloat / (tenure + 1) * 30
    val monthlySpendRate: Float = amounts.sum.toFloat / (tenure + 1) * 30
    
    private val sorted = transactions.sortBy(_.timestamp)
    private val firstHalf = sorted.take(sorted.size / 2)
    private val secondHalf = sorted.drop(sorted.size / 2)
    private val firstAvg = if firstHalf.nonEmpty then firstHalf.map(_.amount).sum / firstHalf.size else 0.0
    private val secondAvg = if secondHalf.nonEmpty then secondHalf.map(_.amount).sum / secondHalf.size else 0.0
    val spendGrowth: Float = (secondAvg - firstAvg).toFloat
    val spendGrowthPct: Float = if firstAvg > 0 then ((secondAvg - firstAvg) / firstAvg).toFloat else 0f
    val frequencyGrowth: Float = (secondHalf.size - firstHalf.size).toFloat
    
    private val recentTxs = transactions.filter(t => (now - t.timestamp) / day < 30)
    val recentTxCount: Float = recentTxs.size.toFloat
    val recentSpend: Float = if recentTxs.nonEmpty then recentTxs.map(_.amount).sum.toFloat else 0f
    val isInactive: Float = if recentTxs.isEmpty then 1f else 0f
    
    val tenureNorm: Float = Math.min(tenure / 365, 1f)
    val activityNorm: Float = transactions.size.toFloat / 100
    val spendNorm: Float = amounts.sum.toFloat / 10000
    val avgMonthlyTx: Float = transactions.size.toFloat / (tenure / 30 + 1)
    val categoryBreadth: Float = categorySpendMap.size.toFloat / 20
    val daysCoverage: Float = uniqueDays / tenure
    
    val estimatedViews: Float = (transactions.size * 2.5).toFloat
    val estimatedReturns: Float = (transactions.size * 0.1).toFloat
    val estimatedCartAbandonment: Float = (transactions.size * 0.05).toFloat
    val estimatedWishlist: Float = (transactions.size * 0.3).toFloat
    val estimatedReviews: Float = (transactions.size * 1.5).toFloat
    val conversionRate: Float = transactions.size.toFloat / (transactions.size * 2)
    val priceSensitivity: Float = (amounts.sum / transactions.size / 100).toFloat
    val categoryConcentration: Float = categories.groupBy(identity).values.map(_.size).max.toFloat / transactions.size
    val purchaseRegularity: Float = if gaps.nonEmpty then 1f / (stdDev(gaps.map(_.toDouble)).toFloat + 1) else 0.5f
    val spendSkew: Float = Math.abs((amounts.sum / transactions.size - medianAmount).toFloat)
    val recencyScore: Float = 1f / (recencyDays + 1)

  private def norm(value: Float, scale: Float): Float =
    Math.max(0f, Math.min(1f, value / scale))

  private def stdDev(xs: Seq[Double]): Double =
    if xs.size <= 1 then 0.0
    else
      val mean = xs.sum / xs.size
      math.sqrt(xs.map(x => math.pow(x - mean, 2)).sum / xs.size)
