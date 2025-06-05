package foton

import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.utility.ImageUtility
import io.computenode.cyfra.dsl.{Algebra, Color}
import io.computenode.cyfra.foton.animation.AnimationRenderer
import io.computenode.cyfra.foton.animation.AnimationRenderer.{Parameters, Scene}
import io.computenode.cyfra.utility.Units.Milliseconds

import java.nio.file.{Path, Paths}
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await

export Algebra.given
export Color.*
export io.computenode.cyfra.dsl.{GSeq, GStruct}
export io.computenode.cyfra.dsl.Math3D.{rotate, lessThan}

/** Define function to be drawn
  */

//private[foton] val connection = new VscodeConnection("localhost", 3000)
//private[foton] inline def outputPath(using f: sourcecode.FileName) =
//  val filename = Path.of(summon[sourcecode.File].value).getFileName.toString
//  Paths.get(s".cyfra/out/$filename.png").toAbsolutePath
//
//val Width = 1024
//val WidthU = Width: UInt32
//val Height = 1024
//val HeightU = Height: UInt32
//
//private[foton] enum RenderingStep(val step: Int, val stepName: String):
//  case CompilingShader extends RenderingStep(1, "Compiling shader")
//  case Rendering extends RenderingStep(2, "Rendering")
//
//private[foton] val renderingSteps = RenderingStep.values.length
//
//extension [A, B](a: A)
//  infix def |>(f: A => B): B = f(a)
//
//object RenderingStep:
//  def toMessage(step: RenderingStep) = RenderingMessage(step.step, renderingSteps, step.stepName)
//
//sealed trait RenderSettings
//
//case class RenderAsImage() extends RenderSettings
//case class RenderAsVideo(frames: Int, duration: Milliseconds)
//
//inline def f(fn: (Float32, Float32) => RGB)(using f: sourcecode.File, settings: RenderSettings = RenderAsImage()) =
//  connection.send(RenderingStep.toMessage(RenderingStep.CompilingShader))
//
//  given GContext = new GContext
//  settings match
//    case RenderAsImage() =>
//
//
//      val gpuFunction: GArray2DFunction[Empty, Vec4[Float32], Vec4[Float32]] = GArray2DFunction(Width, Height, {
//        case (_, (x, y), _) =>
//          val u = x.asFloat / WidthU.asFloat
//          val v = y.asFloat / HeightU.asFloat
//          val res = fn(u, v)
//          (res.x, res.y, res.z, 1f)
//      })
//
//      val data = Vec4FloatMem(Array.fill(Width * Height)((0f,0f,0f,0f)))
//      connection.send(RenderingStep.toMessage(RenderingStep.Rendering))
//
//      val result = Await.result(data.map(gpuFunction), 30.seconds)
//      ImageUtility.renderToImage(result, Width, Height, outputPath)
//      connection.send(RenderedMessage(outputPath.toString))
//
//    case RenderAsVideo(frames, duration) =>
//      connection.send(RenderingStep.toMessage(RenderingStep.Rendering))
//      val scene = new Scene:
//        def duration = duration
//
//      val AnimationRenderer = new AnimationRenderer[Scene, GArray2DFunction[Empty, Vec4[Float32], Vec4[Float32]]](new Parameters:
//        def width = Width
//        def height = Height
//        def framesPerSecond = 30
//      ):
//        protected def renderFrame(scene: Empty, time: Float32, fn: GArray2DFunction[Empty, Vec4[Float32], Vec4[Float32]]): Array[RGBA] =
//          val data = Vec4FloatMem(Array.fill(Width * Height)((0f,0f,0f,0f)))
//          Await.result(data.map(fn), 30.seconds)
//
//        protected def renderFunction(scene: Empty): GArray2DFunction[Empty, Vec4[Float32], Vec4[Float32]] =
//          GArray2DFunction(Width, Height, {
//            case (_, (x, y), _) =>
//              val u = x.asFloat / WidthU.asFloat
//              val v = y.asFloat / HeightU.asFloat
//              val res = fn(u, v)
//              (res.x, res.y, res.z, 1f)
//          })
