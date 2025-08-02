package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

enum Read:
  case ReadBuf(id: Int, buffer: GBuffer[?], index: Int, value: Result)
  case ReadUni(id: Int, uniform: GUniform[?], value: Result)
export Read.*

enum Write:
  case WriteBuf(buffer: GBuffer[?], index: Int, value: Result)
  case WriteUni(uni: GUniform[?], value: Result)
export Write.*
