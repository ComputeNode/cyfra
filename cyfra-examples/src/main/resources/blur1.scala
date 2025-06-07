import io.computenode.cyfra.dsl.GStruct

def blurFunction(radius: Int): GFunction[GStruct.Empty, Float32, Float32] =
  GFunction.from2D(dim):
    case (_, (x, y), image) =>
      def sample(dx: Int32, dy: Int32) : Float32 =
        image.at(x + dx, y + dy)
        
      def blur(radius: Int): Float32 =
        val samples = for
          dx <- -radius to radius
          dy <- -radius to radius
        yield sample(dx, dy)
        samples.reduce(_ + _) / samples.size.toFloat
        
      blur(radius)