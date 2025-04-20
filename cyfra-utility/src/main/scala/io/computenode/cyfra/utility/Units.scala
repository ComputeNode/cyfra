package io.computenode.cyfra.utility

import scala.concurrent.duration.Duration

object Units:
  
  opaque type Milliseconds <: Float = Float
  object Milliseconds:
    def apply(value: Float): Milliseconds = value
    def toFloat(value: Milliseconds): Float = value
    given Conversion[Duration, Milliseconds] = _.toMillis.toFloat
      