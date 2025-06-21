package io.computenode.cyfra.core.aalegacy

import io.computenode.cyfra.core.aalegacy.mem.{GMem, RamGMem}
import io.computenode.cyfra.dsl.Value

import scala.concurrent.Future

trait Executable[H <: Value, R <: Value]:
  def execute(input: GMem[H], output: RamGMem[R, ?]): Future[Unit]
