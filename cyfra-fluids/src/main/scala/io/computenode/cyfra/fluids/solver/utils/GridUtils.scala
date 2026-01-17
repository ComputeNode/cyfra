package io.computenode.cyfra.fluids.solver.utils

import io.computenode.cyfra.dsl.{*, given}

/** Utility functions for 3D grid operations in GPU shaders. */
object GridUtils:
  
  /** Convert 3D coordinates to 1D flattened index.
    * Uses row-major order: index = x + y*N + z*N²
    */
  inline def coord3dToIdx(x: Int32, y: Int32, z: Int32, n: Int32): Int32 =
    x + y * n + z * n * n

  def idxTo3D(idx: Int32, n: Int32): (Int32, Int32, Int32) =
    val z = idx / (n * n)
    val y = (idx / n).mod(n)
    val x = idx.mod(n)
    (x, y, z)
    
  
  /** Check if 3D coordinates are within grid bounds */
  inline def inBounds(x: Int32, y: Int32, z: Int32, n: Int32): GBoolean =
    (x >= 0) && (x < n) && (y >= 0) && (y < n) && (z >= 0) && (z < n)
  
  /** Min for Int32 (Functions.min only supports Float32) */
  inline def minInt32(a: Int32, b: Int32)(using Source): Int32 =
    when(a < b)(a).otherwise(b)
  
  /** Max for Int32 (Functions.max only supports Float32) */
  inline def maxInt32(a: Int32, b: Int32)(using Source): Int32 =
    when(a > b)(a).otherwise(b)
  
  /** Read Vec4 from buffer with bounds checking, return zero if out of bounds */
  def readVec4Safe(buffer: GBuffer[Vec4[Float32]], x: Int32, y: Int32, z: Int32, n: Int32)
                  (using Source): Vec4[Float32] =
    val xClamped = maxInt32(0, minInt32(x, n - 1))
    val yClamped = maxInt32(0, minInt32(y, n - 1))
    val zClamped = maxInt32(0, minInt32(z, n - 1))
    buffer.read(coord3dToIdx(xClamped, yClamped, zClamped, n))
  
  /** Read scalar from buffer with bounds checking */
  def readFloat32Safe(buffer: GBuffer[Float32], x: Int32, y: Int32, z: Int32, n: Int32)
                     (using Source): Float32 =
    val xClamped = maxInt32(0, minInt32(x, n - 1))
    val yClamped = maxInt32(0, minInt32(y, n - 1))
    val zClamped = maxInt32(0, minInt32(z, n - 1))
    buffer.read(coord3dToIdx(xClamped, yClamped, zClamped, n))
  
  /** Trilinear interpolation for Vec4 field.
    * Samples 8 surrounding grid points and blends them.
    * Note: pos only uses x,y,z components
    */
  def trilinearInterpolateVec4(
    buffer: GBuffer[Vec4[Float32]], 
    pos: Vec3[Float32], 
    size: Int32
  )(using Source): Vec4[Float32] =
    val maxPos = (size - 1).asFloat
    val x = clamp(pos.x, 0.0f, maxPos)
    val y = clamp(pos.y, 0.0f, maxPos)
    val z = clamp(pos.z, 0.0f, maxPos)
    
    val x0 = x.asInt
    val y0 = y.asInt
    val z0 = z.asInt
    val x1 = minInt32(x0 + 1, size - 1)
    val y1 = minInt32(y0 + 1, size - 1)
    val z1 = minInt32(z0 + 1, size - 1)
    
    val fx = x - x0.asFloat
    val fy = y - y0.asFloat
    val fz = z - z0.asFloat
    
    val v000 = readVec4Safe(buffer, x0, y0, z0, size)
    val v100 = readVec4Safe(buffer, x1, y0, z0, size)
    val v010 = readVec4Safe(buffer, x0, y1, z0, size)
    val v110 = readVec4Safe(buffer, x1, y1, z0, size)
    val v001 = readVec4Safe(buffer, x0, y0, z1, size)
    val v101 = readVec4Safe(buffer, x1, y0, z1, size)
    val v011 = readVec4Safe(buffer, x0, y1, z1, size)
    val v111 = readVec4Safe(buffer, x1, y1, z1, size)
    
    val fxVec = vec4(fx, fx, fx, fx)
    val fyVec = vec4(fy, fy, fy, fy)
    val fzVec = vec4(fz, fz, fz, fz)
    
    val v00 = mix(v000, v100, fxVec)
    val v10 = mix(v010, v110, fxVec)
    val v01 = mix(v001, v101, fxVec)
    val v11 = mix(v011, v111, fxVec)
    
    val v0 = mix(v00, v10, fyVec)
    val v1 = mix(v01, v11, fyVec)
    
    mix(v0, v1, fzVec)

  /** Trilinear interpolation for Float32 field */
  def trilinearInterpolateFloat32(
    buffer: GBuffer[Float32], 
    pos: Vec3[Float32], 
    size: Int32
  )(using Source): Float32 =
    val maxPos = (size - 1).asFloat
    val x = clamp(pos.x, 0.0f, maxPos)
    val y = clamp(pos.y, 0.0f, maxPos)
    val z = clamp(pos.z, 0.0f, maxPos)
    
    val x0 = x.asInt
    val y0 = y.asInt
    val z0 = z.asInt
    val x1 = minInt32(x0 + 1, size - 1)
    val y1 = minInt32(y0 + 1, size - 1)
    val z1 = minInt32(z0 + 1, size - 1)
    
    val fx = x - x0.asFloat
    val fy = y - y0.asFloat
    val fz = z - z0.asFloat
    
    val v000 = readFloat32Safe(buffer, x0, y0, z0, size)
    val v100 = readFloat32Safe(buffer, x1, y0, z0, size)
    val v010 = readFloat32Safe(buffer, x0, y1, z0, size)
    val v110 = readFloat32Safe(buffer, x1, y1, z0, size)
    val v001 = readFloat32Safe(buffer, x0, y0, z1, size)
    val v101 = readFloat32Safe(buffer, x1, y0, z1, size)
    val v011 = readFloat32Safe(buffer, x0, y1, z1, size)
    val v111 = readFloat32Safe(buffer, x1, y1, z1, size)
    
    val v00 = mix(v000, v100, fx)
    val v10 = mix(v010, v110, fx)
    val v01 = mix(v001, v101, fx)
    val v11 = mix(v011, v111, fx)
    
    val v0 = mix(v00, v10, fy)
    val v1 = mix(v01, v11, fy)
    
    mix(v0, v1, fz)
