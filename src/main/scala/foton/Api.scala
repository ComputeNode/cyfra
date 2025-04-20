package foton

import io.computenode.cyfra.ImageUtility
import io.computenode.cyfra.dsl.{Empty, FloatMem, GArray, GContext, GFunction, MVPContext, Vec4FloatMem}
import io.computenode.cyfra.vscode.VscodeConnection
import io.computenode.cyfra.vscode.VscodeConnection.{RenderedMessage, RenderingMessage}

import java.nio.file.{Path, Paths}
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await

export io.computenode.cyfra.dsl.Algebra.*
export io.computenode.cyfra.dsl.Algebra.given
export io.computenode.cyfra.dsl.Expression.*
export io.computenode.cyfra.dsl.Value.*
export io.computenode.cyfra.utility.Color.*
export io.computenode.cyfra.dsl.Functions.*
export io.computenode.cyfra.dsl.Control.*
export io.computenode.cyfra.dsl.{GSeq, GStruct}
export io.computenode.cyfra.utility.Math3D.{rotate, lessThan}


type RGB = Vec3[Float32]
type RGBA = Vec4[Float32]

/**
 * Define function to be drawn
 */

private[foton] val connection = new VscodeConnection("localhost", 3000)
private[foton] inline def outputPath(using f: sourcecode.FileName) =
  val filename = Path.of(summon[sourcecode.File].value).getFileName.toString
  Paths.get(s".cyfra/out/$filename.png").toAbsolutePath

val Width = 1024
val WidthU = Width: UInt32
val Height = 1024
val HeightU = Height: UInt32

private[foton] enum RenderingStep(val step: Int, val stepName: String):
  case CompilingShader extends RenderingStep(1, "Compiling shader")
  case Rendering extends RenderingStep(2, "Rendering")

private[foton] val renderingSteps = RenderingStep.values.length

extension [A, B](a: A)
  infix def |>(f: A => B): B = f(a)

object RenderingStep:
  def toMessage(step: RenderingStep) = RenderingMessage(step.step, renderingSteps, step.stepName)

inline def f(fn: (Float32, Float32) => RGB)(using f: sourcecode.File) =
  connection.send(RenderingStep.toMessage(RenderingStep.CompilingShader))
  given GContext = new MVPContext

  val gpuFunction: GFunction[Empty, Vec4[Float32], Vec4[Float32]] = GFunction(Width, Height, {
    case (_, (x, y), _) =>
      val u = x.asFloat / WidthU.asFloat
      val v = y.asFloat / HeightU.asFloat
      val res = fn(u, v)
      (res.x, res.y, res.z, 1f)
  })

  val data = Vec4FloatMem(Array.fill(Width * Height)((0f,0f,0f,0f)))

  connection.send(RenderingStep.toMessage(RenderingStep.Rendering))
  val result = Await.result(data.map(gpuFunction), 1.second)

  ImageUtility.renderToImage(result, Width, Height, outputPath)
  connection.send(RenderedMessage(outputPath.toString))



