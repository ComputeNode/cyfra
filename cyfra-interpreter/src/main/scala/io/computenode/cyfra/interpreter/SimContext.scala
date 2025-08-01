package io.computenode.cyfra.interpreter

type Results = Map[InvocId, Result]

extension (r: Results)
  // assumes both results have the same set of keys.
  def join(that: Results)(op: (Result, Result) => Result): Results =
    r.map: (invocId, res) =>
      invocId -> op(res, that(invocId))

case class SimContext(results: Results = Map(), records: Records = Map(), data: SimData = SimData())
