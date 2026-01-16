package io.computenode.cyfra.recommendations.service

import io.computenode.cyfra.dsl.{*, given}
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}
import io.computenode.cyfra.dsl.gio.GIO
import io.computenode.cyfra.dsl.struct.GStruct
import io.computenode.cyfra.core.{GProgram, GCodec}
import io.computenode.cyfra.core.layout.Layout

/** Parameters for similarity search */
case class SimilarityParams(
  numProducts: Int32,
  embeddingDim: Int32,
  queryProductId: Int32
) extends GStruct[SimilarityParams]

object SimilarityParams:
  given GStructSchema[SimilarityParams] = GStructSchema.derived
  
  given GCodec[SimilarityParams, (Int, Int, Int)] with
    def toByteBuffer(buf: java.nio.ByteBuffer, chunk: Array[(Int, Int, Int)]): java.nio.ByteBuffer =
      buf.clear().order(java.nio.ByteOrder.nativeOrder())
      chunk.foreach { case (a, b, c) => buf.putInt(a); buf.putInt(b); buf.putInt(c) }
      buf.flip(); buf
    def fromByteBuffer(buf: java.nio.ByteBuffer, arr: Array[(Int, Int, Int)]): Array[(Int, Int, Int)] =
      arr.indices.foreach(i => arr(i) = (buf.getInt(), buf.getInt(), buf.getInt()))
      buf.rewind(); arr

/** Layout for GPU similarity computation */
case class SimilarityLayout(
  queryEmbedding: GBuffer[Float32],      // [embeddingDim]
  productEmbeddings: GBuffer[Float32],   // [numProducts × embeddingDim]
  similarities: GBuffer[Float32],        // [numProducts] - output cosine similarities
  params: GUniform[SimilarityParams]
) extends Layout

/** GPU program for parallel cosine similarity computation */
object SimilarityProgram:
  
  /** Compute cosine similarity between query and all products
    * 
    * Each GPU thread computes similarity for one product:
    * cosine_similarity = dot(query, product) / (norm(query) * norm(product))
    * 
    * We precompute norms and normalize embeddings offline, so this simplifies to dot product.
    */
  def computeSimilarities(numProducts: Int, embeddingDim: Int): GProgram[Int, SimilarityLayout] =
    GProgram.static[Int, SimilarityLayout](
      layout = _ => SimilarityLayout(
        queryEmbedding = GBuffer[Float32](embeddingDim),
        productEmbeddings = GBuffer[Float32](numProducts * embeddingDim),
        similarities = GBuffer[Float32](numProducts),
        params = GUniform[SimilarityParams]()
      ),
      dispatchSize = identity  // numProducts passed at runtime
    ): layout =>
      val productIdx = GIO.invocationId
      val params = layout.params.read
      
      GIO.when(productIdx < params.numProducts):
        // Compute dot product between query and this product
        val productBase = productIdx * params.embeddingDim
        
        val dotProduct = GSeq.gen[Int32](0, _ + 1).limit(embeddingDim)
          .fold(0.0f, (sum: Float32, dim: Int32) => {
            val queryVal = GIO.read(layout.queryEmbedding, dim)
            val productVal = GIO.read(layout.productEmbeddings, productBase + dim)
            sum + queryVal * productVal
          })
        
        // Write similarity score (assuming normalized embeddings, dot product = cosine similarity)
        GIO.write(layout.similarities, productIdx, dotProduct)
