package io.computenode.cyfra.compiler

import io.computenode.cyfra.compiler.Spirv.{Capability, Op, StorageClass}
import io.computenode.cyfra.compiler.ir.IR
import io.computenode.cyfra.compiler.unit.*
import io.computenode.cyfra.core.expression.Value
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given

class ManagerTest extends munit.FunSuite:
  private def empty = Context(ExtensionManager(), Nil, Nil, TypeManager(), ConstantsManager(), Nil)

  test("nested type and constant requests retain every dependency and reuse lengths"):
    val initial = empty
    val ((length, array, repeated), context) = Ctx.withCapability(initial):
      val length = Ctx.getConstant[UInt32](4)
      val array = Ctx.getType(Value[GList[GList[Float16, 4], 2]], decorate = true)
      val repeated = Ctx.getConstant[UInt32](4)
      (length, array, repeated)
    assertEquals(length.id, repeated.id)
    assert(context.types.cache.contains(TypeManager.Type(Value[Float16].tag)))
    assert(context.types.cache.contains(TypeManager.Type(Value[UInt32].tag)))
    assert(context.types.cache.values.exists(_.id == array.id))
    val inner = context.types.cache(TypeManager.Type(Value[GList[Float16, 4]].tag)).asInstanceOf[IR.SvRef[Unit]]
    assertEquals(inner.operands.last, length)
    assertEquals(context.constants.cache.size, 2)
    assert(context.extensions.capabilitySet(Capability.Float16.opcode))
    assert(initial.types.cache.isEmpty)
    assert(initial.constants.cache.isEmpty)
    assert(initial.extensions.capabilitySet.isEmpty)

  test("pointer registration enables capabilities directly and cache hits add no declarations"):
    val (_, context) = Ctx.withCapability(empty):
      Ctx.getTypePointer(Value[GList[Float16, 8]], StorageClass.StorageBuffer)
      Ctx.getTypePointer(Value[GList[Float16, 8]], StorageClass.Uniform)
    assert(
      Set(Capability.Float16, Capability.StorageBuffer16BitAccess, Capability.UniformAndStorageBuffer16BitAccess)
        .forall(c => context.extensions.capabilitySet(c.opcode)),
    )
    val (_, repeated) = Ctx.withCapability(context):
      Ctx.getTypePointer(Value[GList[Float16, 8]], StorageClass.StorageBuffer)
      Ctx.getTypePointer(Value[GList[Float16, 8]], StorageClass.Uniform)
    assertEquals(repeated, context)

  test("composite constant recursion preserves scalar cache entries"):
    val ((scalar, vector), context) = Ctx.withCapability(empty):
      val vector = Ctx.getConstant[Vec4[Float32]]((1.0f, 2.0f, 1.0f, 2.0f))
      val scalar = Ctx.getConstant[Float32](1.0f)
      (scalar, vector)
    assertEquals(context.constants.cache.size, 3)
    assertEquals(context.constants.block.size, 3)
    assert(vector.asInstanceOf[IR.SvRef[Vec4[Float32]]].operands.contains(scalar))

  test("compilation contexts do not share state and failure leaves the input untouched"):
    val initial = empty
    intercept[IllegalArgumentException]:
      Ctx.withCapability(initial):
        Ctx.getType(Value[Float16])
        throw IllegalArgumentException("abort")
    val (_, next) = Ctx.withCapability(initial):
      Ctx.getConstant[UInt32](7)
    assert(initial.types.cache.isEmpty)
    assert(initial.constants.cache.isEmpty)
    assert(next.extensions.capabilitySet.isEmpty)
    assertEquals(next.constants.cache.size, 1)

  test("extension requests are deduplicated without losing other manager updates"):
    val ((first, second), context) = Ctx.withCapability(empty):
      ExtensionManager.enableExtension("SPV_KHR_16bit_storage")
      val first = ExtensionManager.getExtInstImport("GLSL.std.450")
      TypeManager.getType(Value[Float16])
      ExtensionManager.enableExtension("SPV_KHR_16bit_storage")
      ExtensionManager.enableCapability(Capability.Float16)
      (first, ExtensionManager.getExtInstImport("GLSL.std.450"))
    assertEquals(first.id, second.id)
    assertEquals(context.extensions.extensions.size, 1)
    assertEquals(context.extensions.imports.size, 1)
    assertEquals(context.extensions.capabilities.size, 1)
    assert(context.types.cache.contains(TypeManager.Type(Value[Float16].tag)))

  test("finalizer orders only declarations and preserves annotations, caches and function bodies"):
    import io.computenode.cyfra.compiler.ir.IRs
    import io.computenode.cyfra.compiler.modules.Finalizer
    val (pointer, context) = Ctx.withCapability(empty):
      TypeManager.getType(Value[GList[GList[Float32, 4], 2]], decorate = true)
      ConstantsManager.get((4, 2), Value[Vec2[UInt32]])
      TypeManager.getPointer(Value[GList[GList[Float32, 4], 2]], StorageClass.Workgroup)
    val global = IR.SvRef[Unit](Op.OpVariable, pointer, List(StorageClass.Workgroup))
    val main = IR.SvRef[Unit](Op.OpFunction, Nil)
    val input = CompilationUnit(Metadata(Nil, Compiler.Compute(Seq.empty, (1, 1, 1))), context.copy(suffix = List(global)), List(IRs(main)))
    val finalized = new Finalizer().compile(input)
    val output = finalized.context.output
    val definitions = context.types.block ++ context.constants.block
    val ids = definitions.map(_.id).toSet
    val emitted = output.collect { case ref: IR.SvRef[?] if ids(ref.id) => ref }
    assertEquals(emitted.size, definitions.size)
    assertEquals(emitted.map(_.id).toSet, ids)
    val positions = output.zipWithIndex.map((ir, index) => ir.id -> index).toMap
    emitted.foreach: ref =>
      val dependencies = ref.tpe.toList ++ ref.operands.collect { case dep: IR.RefIR[?] => dep }
      assert(dependencies.forall(dep => positions(dep.id) < positions(ref.id)))
      assert(positions(ref.id) < positions(global.id))
    assert(context.types.decorations.forall(dec => positions(dec.id) < positions(emitted.head.id)))
    assertEquals(finalized.functionBodies, input.functionBodies)
    assertEquals(finalized.context.suffix.last, global)
    assertEquals(finalized.context.types.cache, context.types.cache)
    assertEquals(finalized.context.constants.cache, context.constants.cache)
    assert(finalized.context.types.block.isEmpty)
    assert(finalized.context.constants.block.isEmpty)
