package io.computenode.cyfra.compiler

import io.computenode.cyfra.core.binding.{BindingRef, GBinding}

extension (binding: GBinding[?])
  def id = binding.asInstanceOf[BindingRef[?]].layoutOffset