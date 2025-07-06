package io.computenode.cyfra.core.archive

import io.computenode.cyfra.core.archive.mem.{GMem, RamGMem}
import io.computenode.cyfra.dsl.Value

import scala.concurrent.Future

trait Executable[H <: Value, R <: Value]:
  def execute(input: GMem[H], output: RamGMem[R, ?]): Future[Unit]
