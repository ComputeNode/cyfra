package io.computenode.cyfra.interpreter

type Results = Map[InvocId, Result]

extension (results: Results)
  // assumes both results have the same set of keys.
  def join(that: Results)(op: (Result, Result) => Result): Results =
    results.map: (invocId, res) =>
      invocId -> op(res, that(invocId))

case class SimContext(results: Results = Map(), records: Records, data: SimData = SimData(), profs: List[CoalesceProfile] = Nil)
