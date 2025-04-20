package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.runtime.mem.{GMem, WritableGMem}

import scala.concurrent.Future

trait Executable[H <: Value, R <: Value] {
  def execute(input: GMem[H], output: WritableGMem[R, _]): Future[Unit]
}
