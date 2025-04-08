package io.computenode.cyfra.samples.slides

import io.computenode.cyfra.dsl.Value.{Float32, Int32, Vec4}
import io.computenode.cyfra.dsl.Expression.*
import io.computenode.cyfra.dsl.Value.*
import io.computenode.cyfra.dsl.*

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.{Await, ExecutionContext}
import io.computenode.cyfra.dsl.Algebra.*
import io.computenode.cyfra.dsl.Algebra.given
import io.computenode.cyfra.dsl.given

import scala.concurrent.duration.DurationInt
import io.computenode.cyfra.dsl.Functions.*
import io.computenode.cyfra.dsl.Control.*
import io.computenode.cyfra.dsl.{Empty, GArray2DFunction, GSeq, GStruct, Vec4FloatMem}
import io.computenode.cyfra.{ImageUtility}

import javax.swing.JFrame
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent

@main
def realTimeRendering =
  val dim = 1024
  val frame = new JFrame("Cyfra Live Raytracing")
  frame.setVisible(true)


  def computeImage(xLimit : Int): GArray2DFunction[Empty, Vec4[Float32], Vec4[Float32]] = GArray2DFunction(dim, dim, {
    case (_, (x: Int32, _), _) =>
      when ((x < xLimit)){
        (1f, 1f, 1f, 1f)
      } otherwise{
        (0f, 0f, 0f, 1f)
      }
  })

  var xLimit = 0
  frame.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match {
          case KeyEvent.VK_LEFT  => xLimit = math.max(0, xLimit - 1)
          case KeyEvent.VK_RIGHT => xLimit = math.min(dim, xLimit + 1)
          case _ => ()
        }
      }
    })

  while (true) {
    val mem = Vec4FloatMem(Array.fill(dim * dim)((0f,0f,0f,0f)))
    val result = Await.result(mem.map(computeImage(xLimit)), 1.second)

    val image = ImageUtility.renderBufferedImg(result, dim, dim)
    ImageUtility.displayImageToWindow(image, frame)
    Thread.sleep(20)
  }