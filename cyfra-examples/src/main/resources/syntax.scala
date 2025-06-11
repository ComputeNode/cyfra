
case class FooStruct(
  foo: Int32,
  bar: Float32
) extends GStruct[FooStruct]

GSeq.gen[Int32](0, _ + 1)
  .limit(32)
  .filter(_ > 10)
  .fold[Int32](0, _ + _)