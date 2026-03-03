package io.computenode.cyfra.fotonviz.render

import io.computenode.cyfra.dsl.{*, given}

/** Lighting and material shading functions. */
object Shading:

  /**
   * Blinn-Phong metallic material.
   *
   * @param normal            Surface normal (normalized)
   * @param viewDir           Direction from surface to camera (normalized)
   * @param lightDir          Direction to light source (normalized)
   * @param baseColor         Material base color
   * @param ambient           Ambient light contribution
   * @param diffuseStrength   Diffuse lighting multiplier
   * @param specularStrength  Specular highlight multiplier
   * @param fresnelStrength   Fresnel rim effect strength
   * @return Shaded color
   */
  def metallic(
    normal: Vec3[Float32],
    viewDir: Vec3[Float32],
    lightDir: Vec3[Float32],
    baseColor: Vec3[Float32],
    ambient: Float32 = 0.25f,
    diffuseStrength: Float32 = 0.6f,
    specularStrength: Float32 = 0.9f,
    fresnelStrength: Float32 = 0.4f,
  ): Vec3[Float32] =
    // Diffuse (Lambertian)
    val ndotl = max(normal dot lightDir, 0.0f)
    val diffuse = ndotl * diffuseStrength

    // Specular (Blinn-Phong) - half vector between light and view
    val half = normalize(lightDir + viewDir)
    val ndoth = max(normal dot half, 0.0f)
    val specular = pow64(ndoth) * specularStrength

    // Fresnel rim
    val vdotn = max(viewDir dot normal, 0.0f)
    val fresnel = pow3(1.0f - vdotn) * fresnelStrength

    val intensity = ambient + diffuse + specular + fresnel
    vec3(
      clamp(baseColor.x * intensity, 0.0f, 1.0f),
      clamp(baseColor.y * intensity, 0.0f, 1.0f),
      clamp(baseColor.z * intensity, 0.0f, 1.0f),
    )

  /** Simple diffuse-only shading. */
  def diffuse(
    normal: Vec3[Float32],
    lightDir: Vec3[Float32],
    color: Vec3[Float32],
    ambient: Float32 = 0.2f,
  ): Vec3[Float32] =
    val ndotl = max(normal dot lightDir, 0.0f)
    val intensity = ambient + ndotl * (1.0f - ambient)
    vec3(color.x * intensity, color.y * intensity, color.z * intensity)

  private def pow3(x: Float32): Float32 = x * x * x

  private def pow64(x: Float32): Float32 =
    val x4 = x * x * x * x
    val x16 = x4 * x4 * x4 * x4
    x16 * x16 * x16 * x16
