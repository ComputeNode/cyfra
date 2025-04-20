package io.computenode.cyfra.spirv

private[cyfra] object SpirvConstants:
  val cyfraVendorId: Byte = 44 // https://github.com/KhronosGroup/SPIRV-Headers/blob/main/include/spirv/spir-v.xml#L52

  val localSizeX = 256
  val localSizeY = 1
  val localSizeZ = 1

  val BOUND_VARIABLE = "bound"
  val GLSL_EXT_NAME = "GLSL.std.450"
  val GLSL_EXT_REF = 1
  val TYPE_VOID_REF = 2
  val VOID_FUNC_TYPE_REF = 3
  val MAIN_FUNC_REF = 4
  val GL_GLOBAL_INVOCATION_ID_REF = 5
  val GL_WORKGROUP_SIZE_REF = 6
  val HEADER_REFS_TOP = 7