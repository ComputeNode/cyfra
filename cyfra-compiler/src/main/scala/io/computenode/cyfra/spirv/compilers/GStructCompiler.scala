package io.computenode.cyfra.spirv.compilers

import io.computenode.cyfra.dsl.struct.{GStruct, GStructSchema}
import io.computenode.cyfra.spirv.Context
import io.computenode.cyfra.spirv.Opcodes.*
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.collection.mutable

private[cyfra] object GStructCompiler:

  def defineStructTypes(schemas: List[GStructSchema[?]], context: Context): (List[Words], Context) =
    val sortedSchemas = sortSchemasDag(schemas.distinctBy(_.structTag))
    sortedSchemas.foldLeft((List[Words](), context)) { case ((words, ctx), schema) =>
      (
        words ::: List(
          Instruction(
            Op.OpTypeStruct,
            List(ResultRef(ctx.nextResultId)) ::: schema.fields.map(_._3).map(t => ctx.valueTypeMap(t.tag)).map(ResultRef.apply),
          ),
          Instruction(Op.OpTypePointer, List(ResultRef(ctx.nextResultId + 1), StorageClass.Function, ResultRef(ctx.nextResultId))),
        ),
        ctx.copy(
          nextResultId = ctx.nextResultId + 2,
          valueTypeMap = ctx.valueTypeMap + (schema.structTag.tag -> ctx.nextResultId),
          funPointerTypeMap = ctx.funPointerTypeMap + (ctx.nextResultId -> (ctx.nextResultId + 1)),
        ),
      )
    }

  def getStructNames(schemas: List[GStructSchema[?]], context: Context): (List[Words], Context) =
    schemas.distinctBy(_.structTag).foldLeft((List.empty[Words], context)) { case ((wordsAcc, currCtx), schema) =>
      var structName = schema.structTag.tag.shortName
      var nameSuffix = 0
      while currCtx.names.contains(structName) do
        structName = s"${schema.structTag.tag.shortName}_$nameSuffix"
        nameSuffix += 1
      val structType = context.valueTypeMap(schema.structTag.tag)
      val words = Instruction(Op.OpName, List(ResultRef(structType), Text(structName))) :: schema.fields.zipWithIndex.map {
        case ((name, _, tag), i) =>
          Instruction(Op.OpMemberName, List(ResultRef(structType), IntWord(i), Text(name)))
      }
      val updatedCtx = currCtx.copy(names = currCtx.names + structName)
      (wordsAcc ::: words, updatedCtx)
    }

  private def sortSchemasDag(schemas: List[GStructSchema[?]]): List[GStructSchema[?]] =
    val schemaMap = schemas.map(s => s.structTag.tag -> s).toMap
    val visited = mutable.Set[LightTypeTag]()
    val stack = mutable.Stack[LightTypeTag]()
    val sorted = mutable.ListBuffer[GStructSchema[?]]()

    def visit(tag: LightTypeTag): Unit =
      if !visited.contains(tag) && tag <:< summon[Tag[GStruct[?]]].tag then
        visited += tag
        stack.push(tag)
        schemaMap(tag).fields.map(_._3.tag).foreach(visit)
        sorted += schemaMap(tag)
        stack.pop()

    val roots = schemas.map(_.structTag.tag).filterNot(tag => schemas.exists(_.fields.exists(_._3.tag == tag)))
    roots.foreach(visit)
    sorted.toList
