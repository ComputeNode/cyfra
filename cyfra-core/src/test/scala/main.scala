import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.expression.*
import izumi.reflect.{Tag, TagK, TagKK}
import monocle.syntax.all.*

val v1: Var[Struct] = new Var()

@main
def run(): Unit =
//  v1._1

  val a = (42, (3.14f, 2137))
  val res = a.focus(_._2._1).get

  println(res)

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
