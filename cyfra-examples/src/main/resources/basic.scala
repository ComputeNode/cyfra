
@main
def sample =
  val data = (0 to 256).map(_.toFloat).toArray
  val mem = FloatMem(data)
  
  val gpuFunction = GFunction:
    (value: Float32) => value * 2f
    
  val result = mem.map(gpuFunction).toFloatArray
  println(result.mkString(", "))