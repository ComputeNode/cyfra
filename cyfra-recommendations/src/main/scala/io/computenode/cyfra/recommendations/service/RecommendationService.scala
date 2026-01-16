package io.computenode.cyfra.recommendations.service

import cats.effect.{IO, Ref}
import io.computenode.cyfra.recommendations.model.*
import io.computenode.cyfra.runtime.VkCyfraRuntime
import io.computenode.cyfra.core.GBufferRegion
import io.computenode.cyfra.dsl.binding.{GBuffer, GUniform}

import scala.util.Random

/** Service for GPU-accelerated product recommendations */
class RecommendationService(
  products: Vector[Product],
  embeddingDim: Int
)(using runtime: VkCyfraRuntime):
  
  private val productMap = products.map(p => p.id -> p).toMap
  
  // Flatten all embeddings into single array for GPU upload
  private val flatEmbeddings: Array[Float] = products.flatMap(_.embedding.toArray).toArray
  
  private val similarityProgram = SimilarityProgram.computeSimilarities(products.size, embeddingDim)
  
  /** Find similar products using GPU acceleration
    *
    * @param productId The product to find recommendations for
    * @param limit Maximum number of recommendations
    * @param minScore Minimum similarity score threshold
    * @return Recommended products with similarity scores
    */
  def findSimilar(productId: Long, limit: Int = 10, minScore: Float = 0.0f): IO[(List[SimilarProduct], Long)] =
    productMap.get(productId) match
      case None => IO.raiseError(new IllegalArgumentException(s"Product $productId not found"))
      case Some(queryProduct) =>
        val startTime = System.currentTimeMillis()
        
        for
          // Compute similarities on GPU
          similarities <- computeSimilaritiesGPU(queryProduct.embedding)
          
          // Find top-K similar products (excluding the query product itself)
          topK = similarities.zipWithIndex
            .filter { case (score, idx) => 
              products(idx).id != productId && score >= minScore
            }
            .sortBy(-_._1)
            .take(limit)
            .map { case (score, idx) =>
              SimilarProduct(products(idx), score)
            }
            .toList
          
          elapsed = System.currentTimeMillis() - startTime
        yield (topK, elapsed)
  
  /** Compute similarity scores for query product against all products using GPU */
  private def computeSimilaritiesGPU(queryEmbedding: List[Float]): IO[Array[Float]] =
    IO {
      val numProducts = products.size
      val results = new Array[Float](numProducts)
      
      GBufferRegion.allocate[SimilarityLayout]
        .map { layout =>
          similarityProgram.execute(numProducts, layout)
          layout
        }
        .runUnsafe(
          init = SimilarityLayout(
            queryEmbedding = GBuffer(queryEmbedding.toArray),
            productEmbeddings = GBuffer(flatEmbeddings),
            similarities = GBuffer(results),
            params = GUniform[(Int, Int, Int), SimilarityParams]((numProducts, embeddingDim, 0))
          ),
          onDone = _.similarities.readArray[Float](results)
        )
      
      results
    }
  
  /** Get product by ID */
  def getProduct(productId: Long): Option[Product] = productMap.get(productId)
  
  /** Get all products */
  def getAllProducts: Vector[Product] = products
  
  /** Get service stats */
  def getStats: (Int, Int, Boolean) = (products.size, embeddingDim, true)

object RecommendationService:
  
  /** Create service with synthetically generated products for demo */
  def createDemo(numProducts: Int = 1000, embeddingDim: Int = 128)(using VkCyfraRuntime): IO[RecommendationService] =
    IO {
      val products = generateDemoProducts(numProducts, embeddingDim)
      new RecommendationService(products, embeddingDim)
    }
  
  /** Generate demo products with synthetic embeddings
    *
    * Creates products in clusters representing categories, so similar products
    * have similar embeddings (useful for testing recommendations).
    */
  private def generateDemoProducts(numProducts: Int, embeddingDim: Int): Vector[Product] =
    val random = new Random(42)
    val categories = Vector(
      "Electronics", "Books", "Clothing", "Home & Garden", "Toys",
      "Sports", "Beauty", "Food & Beverage", "Automotive", "Health"
    )
    
    // Create category prototypes (centers in embedding space)
    val categoryPrototypes = categories.map { _ =>
      Array.fill(embeddingDim)(random.nextGaussian().toFloat)
    }
    
    // Normalize prototypes
    categoryPrototypes.foreach(normalizeInPlace)
    
    (0 until numProducts).map { id =>
      val categoryIdx = id % categories.size
      val category = categories(categoryIdx)
      val prototype = categoryPrototypes(categoryIdx)
      
      // Product embedding = category prototype + small noise
      val embedding = prototype.map(_ + random.nextGaussian().toFloat * 0.1f)
      normalizeInPlace(embedding)
      
      val price = 10.0 + random.nextDouble() * 490.0
      
      Product(
        id = id.toLong,
        name = s"${category.take(4)}-Product-$id",
        category = category,
        price = price,
        embedding = embedding.toList
      )
    }.toVector
  
  /** Normalize vector in-place for cosine similarity */
  private def normalizeInPlace(vec: Array[Float]): Unit =
    val norm = math.sqrt(vec.map(x => x * x).sum).toFloat
    if norm > 0.0001f then
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) / norm
        i += 1
