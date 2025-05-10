package io.computenode.cyfra.runtime

import io.computenode.cyfra.dsl.Value
import io.computenode.cyfra.runtime.mem.{GMem, RamGMem}

import scala.concurrent.Future

trait Executable[H <: Value, R <: Value] {
  def execute(input: GMem[H], output: RamGMem[R, _]): Future[Unit]
}
