package io.computenode.cyfra.analytics.viz

import java.awt.{Color, Graphics2D, RenderingHints}
import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.util.Random

/** Generates smooth particle clustering animation.
  *
  * Particles flow in from left and smoothly settle into colored clusters.
  */
object ClusteringAnimation:

  val Width = 800
  val Height = 450
  val NumParticles = 200
  val NumClusters = 5
  val NumFrames = 150
  val OutputDir = "clustering_animation"

  case class Particle(startY: Double, targetX: Double, targetY: Double, memberships: Array[Float], delay: Double)

  val ClusterColors = Array(
    new Color(255, 107, 107), // Coral
    new Color(78, 205, 196), // Teal
    new Color(255, 200, 87), // Gold
    new Color(155, 89, 182), // Purple
    new Color(46, 204, 113), // Green
  )

  val ClusterCenters = Array((620.0, 80.0), (560.0, 200.0), (680.0, 160.0), (600.0, 340.0), (720.0, 280.0))

  def main(args: Array[String]): Unit =
    val dir = new File(OutputDir)
    if !dir.exists() then dir.mkdirs()

    val random = new Random(42)
    val particles = generateParticles(random)

    println(s"Generating $NumFrames frames...")
    (0 until NumFrames).foreach { frame =>
      val image = renderFrame(frame, particles)
      val file = new File(dir, f"frame$frame%03d.png")
      ImageIO.write(image, "png", file)
      if frame % 30 == 0 then println(s"  Frame $frame/$NumFrames")
    }

    println(s"\nDone! Creating GIF...")

  def generateParticles(random: Random): Array[Particle] = (0 until NumParticles).map { i =>
    val cluster = random.nextInt(NumClusters)
    val (cx, cy) = ClusterCenters(cluster)

    val targetX = cx + random.nextGaussian() * 30
    val targetY = cy + random.nextGaussian() * 30
    val startY = 50 + random.nextDouble() * (Height - 100)

    val memberships = new Array[Float](NumClusters)
    memberships(cluster) = 0.6f + random.nextFloat() * 0.35f
    val remaining = 1f - memberships(cluster)
    val secondary = (cluster + 1 + random.nextInt(NumClusters - 1)) % NumClusters
    memberships(secondary) = remaining * 0.6f
    (0 until NumClusters).foreach { c =>
      if c != cluster && c != secondary then memberships(c) = remaining * 0.4f / (NumClusters - 2)
    }

    val delay = i * 0.4 + random.nextDouble() * 10
    Particle(startY, targetX, targetY, memberships, delay)
  }.toArray

  def renderFrame(frame: Int, particles: Array[Particle]): BufferedImage =
    val image = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_ARGB)
    val g = image.createGraphics()

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)

    g.setColor(new Color(18, 18, 24))
    g.fillRect(0, 0, Width, Height)

    drawClusterGlow(g, frame)

    particles.sortBy(p => -p.delay).foreach { p =>
      drawParticle(g, p, frame)
    }

    g.dispose()
    image

  def drawClusterGlow(g: Graphics2D, frame: Int): Unit =
    val glowAlpha = Math.min(25, frame / 2)
    if glowAlpha > 0 then
      ClusterCenters.zip(ClusterColors).foreach { case ((cx, cy), color) =>
        g.setColor(new Color(color.getRed, color.getGreen, color.getBlue, glowAlpha))
        g.fillOval((cx - 55).toInt, (cy - 55).toInt, 110, 110)
      }

  def drawParticle(g: Graphics2D, p: Particle, frame: Int): Unit =
    val t = (frame - p.delay) / 80.0
    if t < -0.3 then return

    val progress = Math.max(0, Math.min(1, t))

    // Ease-in-out with acceleration towards end, then slow settle
    val eased = if progress < 0.7 then
      // Slow start, accelerate
      val p1 = progress / 0.7
      p1 * p1 * 0.7
    else
      // Fast middle, slow settle
      val p2 = (progress - 0.7) / 0.3
      0.7 + (1 - Math.pow(1 - p2, 3)) * 0.3

    val startX = -20.0
    val x = startX + eased * (p.targetX - startX)
    val y = p.startY + eased * (p.targetY - p.startY)

    if x < -30 || x > Width + 30 then return

    // Fade in as particle enters
    val fadeIn = Math.min(1.0, (t + 0.3) * 3)

    // Color transition: white → cluster color
    val colorProgress = Math.max(0, Math.min(1, (progress - 0.3) / 0.5))
    val color =
      if colorProgress <= 0 then new Color(200, 200, 220, (fadeIn * 200).toInt)
      else
        val blended = blendMembershipColor(p.memberships)
        val r = (200 + (blended.getRed - 200) * colorProgress).toInt.max(0).min(255)
        val g = (200 + (blended.getGreen - 200) * colorProgress).toInt.max(0).min(255)
        val b = (220 + (blended.getBlue - 220) * colorProgress).toInt.max(0).min(255)
        new Color(r, g, b, (fadeIn * 220).toInt.min(255))

    // Size grows slightly as it settles
    val size = 5.0 + progress * 3.0

    // Glow for settled particles
    if progress > 0.9 then
      val glowAlpha = ((progress - 0.9) * 10 * 40).toInt.min(40)
      val glow = blendMembershipColor(p.memberships)
      g.setColor(new Color(glow.getRed, glow.getGreen, glow.getBlue, glowAlpha))
      g.fill(new Ellipse2D.Double(x - size * 1.5, y - size * 1.5, size * 3, size * 3))

    g.setColor(color)
    g.fill(new Ellipse2D.Double(x - size / 2, y - size / 2, size, size))

  def blendMembershipColor(memberships: Array[Float]): Color =
    var r, g, b = 0f
    memberships.zip(ClusterColors).foreach { case (weight, color) =>
      r += color.getRed * weight
      g += color.getGreen * weight
      b += color.getBlue * weight
    }
    new Color(r.toInt.min(255).max(0), g.toInt.min(255).max(0), b.toInt.min(255).max(0))
