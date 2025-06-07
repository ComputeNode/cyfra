
case class FooStruct(
  foo: Int32,
  bar: Float32
) extends GStruct[FooStruct]

GSeq.gen[Int32](_, _ + 1)
  .limit(32)
  .map(_ > 10)
  .fold[Int32](0, _ + _)