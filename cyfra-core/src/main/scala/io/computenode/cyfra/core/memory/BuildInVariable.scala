package io.computenode.cyfra.core.memory

import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.types.{UInt32, Vec3}

sealed trait BuildInVariable[T: Value] extends Variable[T]

object BuildInVariable:
  case object GlobalInvocationId extends BuildInVariable[Vec3[UInt32]]
  case object LocalInvocationId extends BuildInVariable[Vec3[UInt32]]
  case object LocalInvocationIndex extends BuildInVariable[UInt32]
  case object WorkgroupId extends BuildInVariable[Vec3[UInt32]]
  case object NumWorkgroups extends BuildInVariable[Vec3[UInt32]]
  case object SubgroupId extends BuildInVariable[UInt32]
  case object SubgroupLocalInvocationId extends BuildInVariable[UInt32]
  case object SubgroupSize extends BuildInVariable[UInt32]
