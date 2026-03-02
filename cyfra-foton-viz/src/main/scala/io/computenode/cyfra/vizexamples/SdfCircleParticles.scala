package io.computenode.cyfra.vizexamples

import io.computenode.cyfra.core.{Allocation, CyfraRuntime, GCodec, GProgram}
import io.computenode.cyfra.core.layout.Layout
import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.fotonviz.interop.{InteropRenderer, SharedBuffer}
import io.computenode.cyfra.runtime.{VkCyfraRuntime, VkInterop}
import io.computenode.cyfra.spirv.compilers.SpirvProgramCompiler.totalStride
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Random

/** Simulation configuration passed to GPU. */
case class SimConfig(
  particleCount: Int32,
  deltaTime: Float32,
  mouseX: Float32,
  mouseY: Float32,
  attractionStrength: Float32,
  mouseRepulsionStrength: Float32,
  particleRepulsionStrength: Float32,
  damping: Float32,
  mouseRadius: Float32,
) extends GStruct[SimConfig]

/** Render configuration passed to GPU. */
case class RenderConfig(
  width: Int32,
  height: Int32,
  particleCount: Int32,
  particleRadius: Float32,
) extends GStruct[RenderConfig]

/** Physics simulation layout with double-buffered positions/velocities. */
case class ParticleSimLayout(
  posVelCurrent: GBuffer[Vec4[Float32]],
  posVelPrevious: GBuffer[Vec4[Float32]],
  targets: GBuffer[Vec2[Float32]],
  config: GUniform[SimConfig],
) derives Layout:
  def swap: ParticleSimLayout = copy(
    posVelCurrent = posVelPrevious,
    posVelPrevious = posVelCurrent,
  )

/** Render layout - reads positions, writes packed RGBA pixels. */
case class ParticleRenderLayout(
  posVel: GBuffer[Vec4[Float32]],
  pixels: GBuffer[UInt32],
  config: GUniform[RenderConfig],
) derives Layout

object SdfCircleParticles:

  val WIDTH = 1280
  val HEIGHT = 1280
  val PARTICLE_COUNT = 800
  val CIRCLE_RADIUS = 0.3f
  val PARTICLE_RADIUS = 0.008f

  /** Physics program with target attraction, mouse repulsion, and particle-particle repulsion. */
  val physicsProgram: GProgram[Int, ParticleSimLayout] =
    GProgram.static[Int, ParticleSimLayout](
      layout = count => ParticleSimLayout(
        posVelCurrent = GBuffer[Vec4[Float32]](count),
        posVelPrevious = GBuffer[Vec4[Float32]](count),
        targets = GBuffer[Vec2[Float32]](count),
        config = GUniform[SimConfig](),
      ),
      dispatchSize = identity,
    ): layout =>
      val idx = GIO.invocationId
      val config = layout.config.read

      GIO.when(idx < config.particleCount):
        val pv = layout.posVelPrevious.read(idx)
        val target = layout.targets.read(idx)

        val posX = pv.x
        val posY = pv.y
        val velX = pv.z
        val velY = pv.w

        // Attraction to target position
        val toTargetX = target.x - posX
        val toTargetY = target.y - posY
        val targetDist = sqrt(toTargetX * toTargetX + toTargetY * toTargetY) + 0.0001f
        val attractX = (toTargetX / targetDist) * config.attractionStrength * targetDist
        val attractY = (toTargetY / targetDist) * config.attractionStrength * targetDist

        // Repulsion from mouse cursor
        val toMouseX = posX - config.mouseX
        val toMouseY = posY - config.mouseY
        val mouseDist = sqrt(toMouseX * toMouseX + toMouseY * toMouseY) + 0.0001f
        val mouseInfluence = config.mouseRadius / (mouseDist * mouseDist * mouseDist + 0.05f)
        val mouseRepelX = (toMouseX / mouseDist) * config.mouseRepulsionStrength * mouseInfluence
        val mouseRepelY = (toMouseY / mouseDist) * config.mouseRepulsionStrength * mouseInfluence

        // Repulsion from other particles (O(N^2) but GPU handles it)
        val particleRepulsion = GSeq
          .gen[Int32](0, i => i + 1)
          .limit(PARTICLE_COUNT)
          .takeWhile(i => i < config.particleCount)
          .fold(vec2(0.0f, 0.0f), (acc: Vec2[Float32], j: Int32) =>
            val other = layout.posVelPrevious.read(j)
            val dx = posX - other.x
            val dy = posY - other.y
            val distSq = dx * dx + dy * dy + 0.0001f
            val dist = sqrt(distSq)
            // Normalized direction
            val nx = dx / dist
            val ny = dy / dist
            // Soft repulsion: 1/dist^2 falloff, zero for self
            val notSelf = when(j === idx)(0.0f).otherwise(1.0f)
            val strength = config.particleRepulsionStrength / (distSq + 0.001f) * notSelf
            val cappedStrength = min(strength, 0.5f)
            vec2(acc.x + nx * cappedStrength, acc.y + ny * cappedStrength),
          )

        val newVelX = (velX + attractX + mouseRepelX + particleRepulsion.x) * config.damping
        val newVelY = (velY + attractY + mouseRepelY + particleRepulsion.y) * config.damping

        val newPosX = posX + newVelX * config.deltaTime
        val newPosY = posY + newVelY * config.deltaTime

        GIO.write(layout.posVelCurrent, idx, vec4(newPosX, newPosY, newVelX, newVelY))

  /** SDF render program - outputs packed RGBA pixels. */
  val renderProgram: GProgram[Int, ParticleRenderLayout] =
    GProgram.static[Int, ParticleRenderLayout](
      layout = pixelCount => ParticleRenderLayout(
        posVel = GBuffer[Vec4[Float32]](PARTICLE_COUNT),
        pixels = GBuffer[UInt32](pixelCount),
        config = GUniform[RenderConfig](),
      ),
      dispatchSize = identity,
    ): layout =>
      val pixelIdx = GIO.invocationId
      val config = layout.config.read

      GIO.when(pixelIdx < config.width * config.height):
        val px = pixelIdx.mod(config.width)
        val py = pixelIdx / config.width

        val u = (px.asFloat / config.width.asFloat) * 2.0f - 1.0f
        val v = (py.asFloat / config.height.asFloat) * 2.0f - 1.0f

        val sdfResult = GSeq
          .gen[Int32](0, i => i + 1)
          .limit(PARTICLE_COUNT)
          .takeWhile(i => i < config.particleCount)
          .fold(1000.0f, (minDist: Float32, i: Int32) =>
            when(minDist < -config.particleRadius):
              minDist  // Early exit if already inside
            .otherwise:
              val pv = layout.posVel.read(i)
              val dx = u - pv.x
              val dy = v - pv.y
              val dist = sqrt(dx * dx + dy * dy) - config.particleRadius
              min(minDist, dist),
          )

        // Anti-alias band scaled to ~10% of particle radius
        val aaBand = config.particleRadius * 0.15f
        val intensity: Int32 = when(sdfResult < 0.0f):
          0: Int32
        .elseWhen(sdfResult < aaBand):
          (sdfResult / aaBand * 255.0f).asInt
        .otherwise:
          255: Int32

        val iu = intensity.unsigned
        val packed = iu | (iu << 8) | (iu << 16) | ((255: UInt32) << 24)
        GIO.write(layout.pixels, pixelIdx, packed)

  case class ParticleData(posX: Float, posY: Float, velX: Float, velY: Float, targetX: Float, targetY: Float)

  def createInitialParticles(count: Int): Array[ParticleData] =
    val rng = new Random(42)
    (0 until count).map: i =>
      val angle = (i.toFloat / count.toFloat) * 2.0f * Math.PI.toFloat
      val targetX = Math.cos(angle).toFloat * CIRCLE_RADIUS
      val targetY = Math.sin(angle).toFloat * CIRCLE_RADIUS
      val startAngle = rng.nextFloat() * 2.0f * Math.PI.toFloat
      val startRadius = rng.nextFloat() * 0.3f
      val startX = Math.cos(startAngle).toFloat * startRadius
      val startY = Math.sin(startAngle).toFloat * startRadius
      ParticleData(startX, startY, 0.0f, 0.0f, targetX, targetY)
    .toArray

  /** Main entry point - runs with Vulkan-GL interop for zero-copy rendering. */
  @main def runSdfCircleParticles(): Unit =
    println("=== SDF Circle Particles (Vulkan-GL Interop) ===")
    println(s"Window: ${WIDTH}x$HEIGHT, Particles: $PARTICLE_COUNT")

    val renderer = InteropRenderer(WIDTH, HEIGHT, "SDF Particles")

    val runtime = new VkCyfraRuntime()
    given CyfraRuntime = runtime

    try
      runtime.withAllocation: allocation =>
        given Allocation = allocation

        val vkDevice = runtime.vulkanDevice
        val vkPhysicalDevice = runtime.vulkanPhysicalDevice

        println("Initializing shared buffer...")
        val sharedBuffer = renderer.initSharedBuffer(vkDevice, vkPhysicalDevice)

        val particles = createInitialParticles(PARTICLE_COUNT)

        val posVelBuffer1 = ByteBuffer.allocateDirect(PARTICLE_COUNT * 16).order(ByteOrder.nativeOrder())
        val posVelBuffer2 = ByteBuffer.allocateDirect(PARTICLE_COUNT * 16).order(ByteOrder.nativeOrder())
        val targetsBuffer = ByteBuffer.allocateDirect(PARTICLE_COUNT * 8).order(ByteOrder.nativeOrder())

        particles.foreach: p =>
          posVelBuffer1.putFloat(p.posX).putFloat(p.posY).putFloat(p.velX).putFloat(p.velY)
          posVelBuffer2.putFloat(p.posX).putFloat(p.posY).putFloat(p.velX).putFloat(p.velY)
          targetsBuffer.putFloat(p.targetX).putFloat(p.targetY)
        posVelBuffer1.rewind()
        posVelBuffer2.rewind()
        targetsBuffer.rewind()

        val configStride = totalStride(summon[GStructSchema[SimConfig]])
        val configBuffer = ByteBuffer.allocateDirect(configStride).order(ByteOrder.nativeOrder())
        val renderConfigStride = totalStride(summon[GStructSchema[RenderConfig]])
        val renderConfigBuffer = ByteBuffer.allocateDirect(renderConfigStride).order(ByteOrder.nativeOrder())

        var simLayout = ParticleSimLayout(
          posVelCurrent = GBuffer[Vec4[Float32]](posVelBuffer1),
          posVelPrevious = GBuffer[Vec4[Float32]](posVelBuffer2),
          targets = GBuffer[Vec2[Float32]](targetsBuffer),
          config = GUniform[SimConfig](configBuffer),
        )
        allocation.submitLayout(simLayout)

        val pixelBufferInit = ByteBuffer.allocateDirect(WIDTH * HEIGHT * 4).order(ByteOrder.nativeOrder())
        for _ <- 0 until WIDTH * HEIGHT do pixelBufferInit.putInt(0xFF808080)
        pixelBufferInit.rewind()

        var renderLayout = ParticleRenderLayout(
          posVel = simLayout.posVelCurrent,
          pixels = GBuffer[UInt32](pixelBufferInit),
          config = GUniform[RenderConfig](renderConfigBuffer),
        )
        allocation.submitLayout(renderLayout)

        val renderConfig = RenderConfig(WIDTH, HEIGHT, PARTICLE_COUNT, PARTICLE_RADIUS)
        summon[GCodec[RenderConfig, RenderConfig]].toByteBuffer(renderConfigBuffer, Array(renderConfig))
        renderConfigBuffer.rewind()
        renderLayout.config.write(renderConfigBuffer)
        allocation.submitLayout(renderLayout)

        // Warm-up frames
        for _ <- 0 until 2 do
          val simConfig = SimConfig(PARTICLE_COUNT, 0.001f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)
          summon[GCodec[SimConfig, SimConfig]].toByteBuffer(configBuffer, Array(simConfig))
          configBuffer.rewind()
          simLayout.config.write(configBuffer)
          allocation.submitLayout(simLayout)
          physicsProgram.execute(PARTICLE_COUNT, simLayout)
          renderProgram.execute(WIDTH * HEIGHT, renderLayout)
          simLayout = simLayout.swap
          renderLayout = renderLayout.copy(posVel = simLayout.posVelCurrent)

        // Reset particles after warm-up
        posVelBuffer1.rewind()
        posVelBuffer2.rewind()
        particles.foreach: p =>
          posVelBuffer1.putFloat(p.posX).putFloat(p.posY).putFloat(p.velX).putFloat(p.velY)
          posVelBuffer2.putFloat(p.posX).putFloat(p.posY).putFloat(p.velX).putFloat(p.velY)
        posVelBuffer1.rewind()
        posVelBuffer2.rewind()
        simLayout.posVelCurrent.write(posVelBuffer1)
        simLayout.posVelPrevious.write(posVelBuffer2)
        allocation.submitLayout(simLayout)

        val (vkQueue, commandPool) = VkInterop.getQueueAndCommandPool(allocation)

        var frameIndex = 0
        var lastTime = System.nanoTime()
        val startTime = lastTime

        println("Starting render loop...")

        while !renderer.shouldClose do
          renderer.pollEvents()
          renderer.updateMousePosition()

          val currentTime = System.nanoTime()
          val deltaTime = Math.min((currentTime - lastTime) / 1_000_000_000.0f, 0.05f)
          lastTime = currentTime

          val mx = renderer.normalizedMouseX
          val my = renderer.normalizedMouseY

          val simConfig = SimConfig(
            particleCount = PARTICLE_COUNT,
            deltaTime = deltaTime,
            mouseX = mx,
            mouseY = my,
            attractionStrength = 4.0f,
            mouseRepulsionStrength = 0.4f,
            particleRepulsionStrength = 0.0003f,  // Inter-particle repulsion
            damping = 0.94f,
            mouseRadius = 0.15f,
          )
          configBuffer.clear()
          summon[GCodec[SimConfig, SimConfig]].toByteBuffer(configBuffer, Array(simConfig))
          configBuffer.rewind()
          simLayout.config.write(configBuffer)
          allocation.submitLayout(simLayout)

          physicsProgram.execute(PARTICLE_COUNT, simLayout)
          renderLayout = renderLayout.copy(posVel = simLayout.posVelCurrent)
          renderProgram.execute(WIDTH * HEIGHT, renderLayout)
          allocation.submitLayout(renderLayout)

          val srcBufferHandle = VkInterop.getBufferHandle(renderLayout.pixels)
          sharedBuffer.copyFrom(srcBufferHandle, vkQueue, commandPool)

          renderer.render()
          simLayout = simLayout.swap

          if frameIndex % 60 == 0 && frameIndex > 0 then
            val elapsed = (currentTime - startTime) / 1_000_000_000.0f
            val fps = if elapsed > 0 then frameIndex / elapsed else 0
            println(f"Frame $frameIndex, FPS: $fps%.1f")

          frameIndex += 1

    finally
      org.lwjgl.vulkan.VK10.vkDeviceWaitIdle(runtime.vulkanDevice)
      renderer.close()
      runtime.close()

    println("Done!")
