package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

type TreeId = Int
type IdleDuration = Int
type Cache = Map[TreeId, Result]
type Idles = Map[TreeId, IdleDuration]

case class Record(cache: Cache = Map(), writes: List[Write] = Nil, reads: List[Read] = Nil, idles: Idles = Map()):
  def addRead(read: Read): Record = read match
    case ReadBuf(_, _, _, _) => copy(reads = read :: reads)
    case ReadUni(_, _, _)    => copy(reads = read :: reads)

  def addWrite(write: Write): Record = write match
    case WriteBuf(_, _, _) => copy(writes = write :: writes)
    case WriteUni(_, _)    => copy(writes = write :: writes)

  def addResult(treeid: TreeId, res: Result) = copy(cache = cache.updated(treeid, res))
  def updateIdles(treeid: TreeId) = copy(idles = idles.updated(treeid, idles.getOrElse(treeid, 0) + 1))

type InvocId = Int
type Records = Map[InvocId, Record]

object Records:
  def apply(invocIds: Seq[InvocId]): Records = invocIds.map(invocId => invocId -> Record()).toMap

extension (records: Records)
  def updateResults(treeid: TreeId, results: Results): Records =
    records.map: (invocId, record) =>
      results.get(invocId) match
        case None         => invocId -> record
        case Some(result) => invocId -> record.addResult(treeid, result)

  def addWrites(writes: Map[InvocId, Write]) =
    records.map: (invocId, record) =>
      writes.get(invocId) match
        case Some(write) => invocId -> record.addWrite(write)
        case None        => invocId -> record

  def updateIdles(rootTreeId: TreeId) = records.view.mapValues(_.updateIdles(rootTreeId)).toMap
