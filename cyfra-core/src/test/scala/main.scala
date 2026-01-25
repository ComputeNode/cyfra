import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.*
import io.computenode.cyfra.core.expression.Focus.*
import izumi.reflect.{Tag, TagK, TagKK}

val v1: Var[Struct] = new Var()
val v2: Var[RuntimeArray[Inner]] = new Var()

@main
def run(): Unit =
  val a = v1.focus(_._2._1)
  val a_res: Focus[Float32] = FocusConstant[Inner, Float32](FocusConstant[Struct, Inner](v1, 2), 1)
  assert(a == a_res)

  val b = v2.focus(_.at(10)._2)
  val b_res: Focus[Int32] = FocusConstant[Inner, Int32](FocusConstant[RuntimeArray[Inner], Inner](v2, 10), 2)
  assert(b == b_res)

  val i: Int32 = Int32(9)

  val c = v2.focus(_.at(i))
  val c_res: Focus[Inner] = FocusDynamic[RuntimeArray[Inner], Inner](v2, i)
  assert(c == c_res)

  println("dupa" + a)

private def eNa[T: Value](block: ExpressionBlock[?], exp: Expression[T]): T =
  exp.v.extract(block.add(exp))

type Inner = (Float32, UInt32)
type Struct = (Int32, Inner)

given Value[Inner] = new Value:
  protected def extractUnsafe(ir: ExpressionBlock[Inner]): Inner =
    val a = Expression.Composite[Inner, 0](ir.result, 0)
    val b = Expression.Composite[Inner, 1](ir.result, 1)
    (eNa(ir, a), eNa(ir, b))

  def tag: Tag[Inner] = Tag[Inner]
  def baseTag: Option[TagK[?]] = Some(Tag[Tuple2].asInstanceOf[TagK[?]])
  def composite: List[Value[?]] = List(Value[Float32], Value[UInt32])

given Value[Struct] = new Value:
  protected def extractUnsafe(ir: ExpressionBlock[Struct]): Struct =
    val a = Expression.Composite[Struct, 0](ir.result, 0)
    val b = Expression.Composite[Struct, 1](ir.result, 1)
    (eNa(ir, a), b.v.extract(ir.add(b)))

  def tag: Tag[Struct] = Tag[Struct]
  def baseTag: Option[TagK[?]] = Some(Tag[Tuple2].asInstanceOf[TagK[?]])
  def composite: List[Value[?]] = List(Value[Int32], Value[Inner])
