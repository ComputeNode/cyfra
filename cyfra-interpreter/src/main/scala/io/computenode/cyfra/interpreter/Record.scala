package io.computenode.cyfra.interpreter

import io.computenode.cyfra.dsl.{*, given}
import binding.{GBuffer, GUniform}

type TreeId = Int
type Cache = Map[TreeId, Result]

type InvocId = Int
type Records = Map[InvocId, Record]

case class Record(cache: Cache = Map(), writes: List[Write] = Nil, reads: List[Read] = Nil):
  def addRead(read: Read): Record = read match
    case ReadBuf(_, _, _, _) => copy(reads = read :: reads)
    case ReadUni(_, _, _)    => copy(reads = read :: reads)

  def addWrite(write: Write): Record = write match
    case WriteBuf(_, _, _, _) => copy(writes = write :: writes)
    case WriteUni(_, _, _)    => copy(writes = write :: writes)

  def addResult(treeId: TreeId, res: Result) = copy(cache = cache.updated(treeId, res))

extension (records: Records)
  def updateResults(treeid: TreeId, results: Results): Records =
    records.map: (invocId, record) =>
      results.get(invocId) match
        case None        => invocId -> record
        case Some(value) => invocId -> record.addResult(treeid, value)
