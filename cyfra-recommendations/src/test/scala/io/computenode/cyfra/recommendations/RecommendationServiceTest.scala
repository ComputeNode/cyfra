package io.computenode.cyfra.recommendations

import cats.effect.{IO, Resource}
import io.computenode.cyfra.recommendations.model.Product
import io.computenode.cyfra.recommendations.service.RecommendationService
import io.computenode.cyfra.runtime.VkCyfraRuntime
import munit.CatsEffectSuite

class RecommendationServiceTest extends CatsEffectSuite:
  
  // Fixture for VkCyfraRuntime
  val runtimeResource: Resource[IO, VkCyfraRuntime] =
    Resource.make(IO(VkCyfraRuntime()))(rt => IO(rt.close()))
  
  val runtimeFixture = ResourceSuiteLocalFixture("runtime", runtimeResource)
  
  override def munitFixtures = List(runtimeFixture)
  
  test("RecommendationService should load demo products") {
    given VkCyfraRuntime = runtimeFixture()
    
    RecommendationService.createDemo(numProducts = 100, embeddingDim = 128).map { service =>
      assertEquals(service.getAllProducts.size, 100)
    }
  }
  
  test("findSimilar should return recommendations for valid product") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      service <- RecommendationService.createDemo(numProducts = 100, embeddingDim = 128)
      (recommendations, computeTime) <- service.findSimilar(productId = 0L, limit = 10)
    yield
      // Should return recommendations
      assert(recommendations.nonEmpty, "Should return recommendations")
      assert(recommendations.size <= 10, "Should respect limit")
      
      // Recommendations should not include the query product itself
      assert(recommendations.forall(_.product.id != 0L), "Should not include query product")
      
      // Similarity scores should be in [0, 1] range (cosine similarity)
      assert(recommendations.forall(r => r.similarityScore >= 0.0f && r.similarityScore <= 1.0f),
        "Similarity scores should be in [0, 1] range")
      
      // Should be sorted by similarity (descending)
      val scores = recommendations.map(_.similarityScore)
      assertEquals(scores, scores.sorted.reverse, "Should be sorted by similarity descending")
      
      // Should complete in reasonable time (GPU should be fast)
      assert(computeTime < 1000, s"Should complete in <1s, took ${computeTime}ms")
  }
  
  test("findSimilar should find products in same category more similar") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      service <- RecommendationService.createDemo(numProducts = 100, embeddingDim = 128)
      queryProduct <- IO(service.getProduct(0L).get)
      (recommendations, _) <- service.findSimilar(productId = 0L, limit = 5)
    yield
      // Most similar products should be in same category
      val sameCategoryCount = recommendations.count(_.product.category == queryProduct.category)
      assert(sameCategoryCount >= 3, 
        s"At least 3 of top 5 should be in same category, found $sameCategoryCount")
  }
  
  test("findSimilar should respect minScore threshold") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      service <- RecommendationService.createDemo(numProducts = 100, embeddingDim = 128)
      (recommendations, _) <- service.findSimilar(productId = 0L, limit = 100, minScore = 0.5f)
    yield
      // All recommendations should meet threshold
      assert(recommendations.forall(_.similarityScore >= 0.5f),
        "All recommendations should meet minScore threshold")
  }
  
  test("findSimilar should fail for non-existent product") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      service <- RecommendationService.createDemo(numProducts = 100, embeddingDim = 128)
      result <- service.findSimilar(productId = 9999L, limit = 10).attempt
    yield
      assert(result.isLeft, "Should fail for non-existent product")
      val error = result.left.toOption.get
      assert(error.isInstanceOf[IllegalArgumentException], "Should be IllegalArgumentException")
      assert(error.getMessage.contains("not found"), "Error message should mention 'not found'")
  }
  
  test("getProduct should return correct product") {
    given VkCyfraRuntime = runtimeFixture()
    
    RecommendationService.createDemo(numProducts = 100, embeddingDim = 128).map { service =>
      val productOpt = service.getProduct(42L)
      assert(productOpt.isDefined, "Should find product 42")
      assertEquals(productOpt.get.id, 42L, "Should return correct product")
    }
  }
  
  test("getProduct should return None for non-existent product") {
    given VkCyfraRuntime = runtimeFixture()
    
    RecommendationService.createDemo(numProducts = 100, embeddingDim = 128).map { service =>
      val productOpt = service.getProduct(9999L)
      assert(productOpt.isEmpty, "Should return None for non-existent product")
    }
  }
  
  test("GPU computation should be deterministic") {
    given VkCyfraRuntime = runtimeFixture()
    
    for
      service <- RecommendationService.createDemo(numProducts = 50, embeddingDim = 128)
      (recs1, _) <- service.findSimilar(productId = 0L, limit = 10)
      (recs2, _) <- service.findSimilar(productId = 0L, limit = 10)
    yield
      // Same query should return same results
      assertEquals(recs1.map(_.product.id), recs2.map(_.product.id),
        "GPU computation should be deterministic")
      
      // Similarity scores should match
      val scores1 = recs1.map(_.similarityScore)
      val scores2 = recs2.map(_.similarityScore)
      scores1.zip(scores2).foreach { case (s1, s2) =>
        assert(math.abs(s1 - s2) < 0.0001f, s"Scores should match: $s1 vs $s2")
      }
  }
