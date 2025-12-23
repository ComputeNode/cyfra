package io.computenode.cyfra.foton.rt.animation

import io.computenode.cyfra.dsl.archive.Value.Float32
import io.computenode.cyfra.foton.animation.AnimationFunctions.AnimationInstant
import io.computenode.cyfra.foton.animation.AnimationRenderer
import io.computenode.cyfra.foton.rt.shapes.Shape
import io.computenode.cyfra.foton.rt.{Camera, Scene}
import io.computenode.cyfra.utility.Units.Milliseconds

class AnimatedScene(val shapes: AnimationInstant ?=> List[Shape], val camera: AnimationInstant ?=> Camera, val duration: Milliseconds)
    extends AnimationRenderer.Scene:
  def at(time: Float32): Scene =
    given AnimationInstant = AnimationInstant(time)
    Scene(shapes, camera)
