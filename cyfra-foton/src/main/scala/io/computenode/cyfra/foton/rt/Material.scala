package io.computenode.cyfra.foton.rt

import io.computenode.cyfra.dsl.archive.struct.GStruct

case class Material(
  color: Vec3[Float32],
  emissive: Vec3[Float32],
  percentSpecular: Float32 = 0f,
  roughness: Float32 = 0f,
  specularColor: Vec3[Float32] = vec3(0f),
  indexOfRefraction: Float32 = 1f,
  refractionChance: Float32 = 0f,
  refractionRoughness: Float32 = 0f,
  refractionColor: Vec3[Float32] = vec3(0f),
) extends GStruct[Material]

object Material:
  val Zero = Material(vec3(0f), vec3(0f))
