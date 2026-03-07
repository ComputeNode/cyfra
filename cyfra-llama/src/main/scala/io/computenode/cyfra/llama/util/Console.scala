package io.computenode.cyfra.llama.util

/** Terminal color utilities with an orange/amber theme.
  * 
  * Color palette follows a warm, cohesive aesthetic:
  *   - Amber (primary): Headlines, branding
  *   - Rust (accent): User prompts, interactive elements  
  *   - Stone (muted): Stats, metadata, secondary info
  *   - Default: AI output for maximum readability
  */
object Console:
  
  // ANSI escape codes
  private val Esc = "\u001b["
  private val Reset = s"${Esc}0m"
  
  // Primary palette - warm orange/amber tones
  private val Amber     = s"${Esc}38;5;214m"   // Bright warm amber - headers
  private val Rust      = s"${Esc}38;5;172m"   // Muted rust orange - prompts
  private val Terracotta = s"${Esc}38;5;208m"  // Rich orange - emphasis
  
  // Neutral palette
  private val Stone     = s"${Esc}38;5;245m"   // Medium gray - metadata
  private val Dim       = s"${Esc}38;5;240m"   // Dark gray - subtle
  
  // Text styles
  private val Bold      = s"${Esc}1m"
  
  // Semantic formatters
  def title(s: String): String = s"$Bold$Amber$s$Reset"
  def subtitle(s: String): String = s"$Rust$s$Reset"
  def prompt(s: String): String = s"$Bold$Rust$s$Reset"
  def accent(s: String): String = s"$Terracotta$s$Reset"
  def muted(s: String): String = s"$Stone$s$Reset"
  def dim(s: String): String = s"$Dim$s$Reset"
  
  // Decorated elements
  def divider(width: Int = 40): String = dim("─" * width)
  def bullet: String = s"$Rust▸$Reset"
  
  // Formatted output helpers
  def stat(label: String, value: String): String = 
    s"${muted(label)} ${accent(value)}"
  
  def statLine(label: String, value: String): String =
    s"  ${muted(label)} $value"
