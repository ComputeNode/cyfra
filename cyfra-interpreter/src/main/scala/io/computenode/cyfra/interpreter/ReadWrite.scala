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
  case RaceCondition(profile: Profile)
  case Coalesced(startAddress: Int, endAddress: Int, profile: Profile)
  case NotCoalesced(profile: Profile)
import CoalesceProfile.*

object CoalesceProfile:
  def apply(addresses: Seq[Int], profile: Profile): CoalesceProfile =
    val length = addresses.length
    val distinct = addresses.distinct.length == length
    if length == 0 then NotCoalesced(profile)
    else if !distinct then RaceCondition(profile)
    else
      val (start, end) = (addresses.min, addresses.max)
      val coalesced = end - start + 1 == length
      if coalesced then Coalesced(start, end, profile)
      else NotCoalesced(profile)
