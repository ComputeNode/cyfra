package io.computenode.cyfra.satellite.change

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.library.Functions

/** Change detection algorithms for identifying temporal changes
  * 
  * These detect rapid changes in vegetation indicating:
  * - Logging/deforestation
  * - Wildfires
  * - Land clearing
  * - Post-disaster damage
  */
object ChangeDetection:
  
  /** Difference NBR (dNBR) - Standard burn severity metric
    * 
    * Used by USGS and other agencies for fire mapping
    * 
    * Formula: NBR_before - NBR_after
    * 
    * Classification (USGS):
    * - Enhanced Regrowth: < -0.25
    * - Unburned: -0.25 to 0.1
    * - Low Severity: 0.1 to 0.27
    * - Moderate-Low: 0.27 to 0.44
    * - Moderate-High: 0.44 to 0.66
    * - High Severity: > 0.66
    * 
    * @param nbrBefore NBR value before fire
    * @param nbrAfter NBR value after fire
    * @return dNBR value (positive = burn)
    */
  def differenceNBR(nbrBefore: Float32, nbrAfter: Float32): Float32 =
    nbrBefore - nbrAfter
  
  /** Relative difference NBR (RdNBR) - Normalized burn severity
    * 
    * Better for comparing across different vegetation types
    * 
    * Formula: dNBR / sqrt(abs(NBR_before))
    * 
    * @param nbrBefore NBR before fire
    * @param nbrAfter NBR after fire
    * @return RdNBR value
    */
  def relativeDifferenceNBR(nbrBefore: Float32, nbrAfter: Float32): Float32 =
    val dnbr = nbrBefore - nbrAfter
    val denominator = Functions.sqrt(Functions.abs(nbrBefore))
    
    when(denominator < 0.001f)(
      0.0f
    ).otherwise(
      dnbr / denominator
    )
  
  /** NDVI drop - Detect vegetation loss (logging, clearing)
    * 
    * Sudden drops in NDVI indicate:
    * - Logging/deforestation
    * - Land clearing
    * - Severe drought
    * - Disease outbreak
    * 
    * @param ndviBefore NDVI before change
    * @param ndviAfter NDVI after change
    * @return Change magnitude (positive = loss)
    */
  def ndviDrop(ndviBefore: Float32, ndviAfter: Float32): Float32 =
    ndviBefore - ndviAfter
  
  /** Relative NDVI change - Percentage change
    * 
    * @param ndviBefore NDVI before
    * @param ndviAfter NDVI after
    * @return Relative change (-1 to 1)
    */
  def relativeNDVIChange(ndviBefore: Float32, ndviAfter: Float32): Float32 =
    when(Functions.abs(ndviBefore) < 0.001f)(
      when(Functions.abs(ndviAfter) < 0.001f)(
        0.0f
      ).otherwise(
        1.0f
      )
    ).otherwise(
      (ndviAfter - ndviBefore) / Functions.abs(ndviBefore)
    )
  
  /** Composite change score for disturbance detection
    * 
    * Combines multiple indices for robust detection:
    * - NDVI drop (vegetation loss)
    * - NBR drop (fire indicator)
    * - Magnitude of change
    * 
    * Score > 0.5: Likely disturbance
    * Score > 0.7: High confidence disturbance
    * 
    * @param ndviBefore NDVI before
    * @param ndviAfter NDVI after
    * @param nbrBefore NBR before
    * @param nbrAfter NBR after
    * @return Composite disturbance score (0-1)
    */
  def disturbanceScore(
      ndviBefore: Float32,
      ndviAfter: Float32,
      nbrBefore: Float32,
      nbrAfter: Float32
  ): Float32 =
    val ndviChange = ndviBefore - ndviAfter
    val nbrChange = nbrBefore - nbrAfter
    
    // Normalize changes to 0-1
    val ndviComponent = when(ndviChange > 0.0f)(
      Functions.min(ndviChange * 2.0f, 1.0f)
    ).otherwise(0.0f)
    
    val nbrComponent = when(nbrChange > 0.0f)(
      Functions.min(nbrChange * 1.5f, 1.0f)
    ).otherwise(0.0f)
    
    // Weighted combination
    (ndviComponent * 0.6f) + (nbrComponent * 0.4f)
  
  /** Classify change type based on index behavior
    * 
    * Returns classification code:
    * 0 = No change
    * 1 = Fire (high dNBR, moderate NDVI drop)
    * 2 = Logging (high NDVI drop, lower dNBR)
    * 3 = Regrowth (negative changes)
    * 4 = Other disturbance
    * 
    * @param ndviChange NDVI difference (before - after)
    * @param nbrChange NBR difference (before - after)
    * @return Classification code
    */
  def classifyChangeType(ndviChange: Float32, nbrChange: Float32): Int32 =
    // Thresholds
    val fireNBRThreshold = 0.27f
    val loggingNDVIThreshold = 0.25f
    val minChangeThreshold = 0.1f
    
    when(ndviChange < -0.1f && nbrChange < -0.1f)(
      3 // Regrowth
    ).elseWhen(nbrChange > fireNBRThreshold && ndviChange > 0.1f)(
      1 // Fire (high dNBR)
    ).elseWhen(ndviChange > loggingNDVIThreshold && nbrChange < fireNBRThreshold)(
      2 // Logging (high NDVI drop, lower NBR change)
    ).elseWhen(ndviChange > minChangeThreshold || nbrChange > minChangeThreshold)(
      4 // Other disturbance
    ).otherwise(
      0 // No significant change
    )
  
  /** Burn severity classification based on dNBR (USGS standards)
    * 
    * @param dnbr Difference NBR value
    * @return Severity code (0-6)
    */
  def classifyBurnSeverity(dnbr: Float32): Int32 =
    when(dnbr < -0.25f)(
      0 // Enhanced regrowth
    ).elseWhen(dnbr < 0.1f)(
      1 // Unburned
    ).elseWhen(dnbr < 0.27f)(
      2 // Low severity
    ).elseWhen(dnbr < 0.44f)(
      3 // Moderate-low severity
    ).elseWhen(dnbr < 0.66f)(
      4 // Moderate-high severity
    ).otherwise(
      5 // High severity
    )
  
  /** Deforestation confidence score
    * 
    * High score indicates likely logging/clearing
    * 
    * @param ndviChange NDVI drop
    * @param ndviBefore Original NDVI (should be high for forest)
    * @return Confidence score (0-1)
    */
  def deforestationConfidence(ndviChange: Float32, ndviBefore: Float32): Float32 =
    // Only high confidence if:
    // 1. Large NDVI drop
    // 2. Started as healthy vegetation
    val wasForested = when(ndviBefore > 0.5f)(1.0f).otherwise(0.0f)
    val changeMagnitude = Functions.min(ndviChange * 2.5f, 1.0f)
    
    when(ndviChange > 0.2f)(
      wasForested * changeMagnitude
    ).otherwise(0.0f)
  
  /** Detect rapid changes (within short time window)
    * 
    * Useful for detecting fires vs. gradual changes
    * 
    * @param change Change magnitude
    * @param timeWindowMonths Number of months between images
    * @return Rate of change (change per month)
    */
  def changeRate(change: Float32, timeWindowMonths: Float32): Float32 =
    when(timeWindowMonths > 0.1f)(
      change / timeWindowMonths
    ).otherwise(0.0f)

