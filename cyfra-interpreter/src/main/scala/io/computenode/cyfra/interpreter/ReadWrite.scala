package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

enum Read:
  case ReadBuf(id: Int, buffer: GBuffer[?], index: Int, value: Result)
  case ReadUni(id: Int, uniform: GUniform[?], value: Result)
export Read.*

enum Write:
  case WriteBuf(id: Int, buffer: GBuffer[?], index: Int, value: Result)
  case WriteUni(id: Int, uni: GUniform[?], value: Result)
export Write.*
