
def juliaSet(uv: Vec2[Float32]): Int32 =
  GSeq.gen(uv, next = v => (
      (v.x * v.x) - (v.y * v.y),
      2.0f * v.x * v.y,
    ) + const
  ).limit(RECURSION_LIMIT)
    .map(length)
    .takeWhile(_ < 2.0f)
    .count
  