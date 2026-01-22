package io.computenode.cyfra.spirv

private[cyfra] object SpirvConstants:
  val cyfraVendorId: Byte = 44 // https://github.com/KhronosGroup/SPIRV-Headers/blob/main/include/spirv/spir-v.xml#L52

  val localSizeX = 256
  val localSizeY = 1
  val localSizeZ = 1

  val BOUND_VARIABLE = "bound"
  val GLSL_EXT_NAME = "GLSL.std.450"
  val NON_SEMANTIC_DEBUG_PRINTF = "NonSemantic.DebugPrintf"
  val GLSL_EXT_REF = 1
  val TYPE_VOID_REF = 2
  val VOID_FUNC_TYPE_REF = 3
  val MAIN_FUNC_REF = 4
  val GL_GLOBAL_INVOCATION_ID_REF = 5
  val GL_WORKGROUP_SIZE_REF = 6
  val DEBUG_PRINTF_REF = 7
  val GL_LOCAL_INVOCATION_ID_REF = 8
  val GL_LOCAL_INVOCATION_INDEX_REF = 9
  val GL_WORKGROUP_ID_REF = 10
  val GL_NUM_WORKGROUPS_REF = 11
  val GL_SUBGROUP_ID_REF = 12
  val GL_SUBGROUP_LOCAL_INVOCATION_ID_REF = 13
  val GL_SUBGROUP_SIZE_REF = 14

  val HEADER_REFS_TOP = 15
