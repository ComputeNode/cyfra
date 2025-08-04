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

enum Profile:
  case ReadProfile(treeid: TreeId, addresses: Seq[Int])
  case WriteProfile(buffer: GBuffer[?], addresses: Seq[Int])
export Profile.*

enum CoalesceProfile:
  case Coalesced(startAddress: Int, endAddress: Int, profile: Profile)
  case NotCoalesced(profile: Profile)
import CoalesceProfile.*

object CoalesceProfile:
  def apply(addresses: Seq[Int], profile: Profile): CoalesceProfile =
    val (start, end) = (addresses.min, addresses.max)
    val coalesced = end - start + 1 == addresses.length
    if coalesced then Coalesced(start, end, profile) else NotCoalesced(profile)
