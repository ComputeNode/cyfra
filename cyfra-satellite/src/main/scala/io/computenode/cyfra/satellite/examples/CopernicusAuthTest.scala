package io.computenode.cyfra.satellite.examples

import io.computenode.cyfra.satellite.data.{CopernicusAuth, CopernicusOData}
import java.time.LocalDate

/** Simple test to verify Copernicus authentication works */
@main def copernicusAuthTest(): Unit = {
  println("=" * 80)
  println("Copernicus Authentication Test")
  println("=" * 80)
  println()

  // Get credentials
  val (username, password, clientId, clientSecret) = CopernicusAuth.getCredentialsFromEnv()

  if (username.isEmpty && clientId.isEmpty) {
    println("❌ No credentials found!")
    println("   Please configure cyfra-satellite/copernicus-credentials.properties")
    return
  }

  println("✓ Credentials loaded")
  println()

  // Test authentication
  println("Testing OAuth authentication...")
  CopernicusAuth.getAccessToken(clientId, clientSecret, username, password) match {
    case scala.util.Success(token) =>
      println(s"✅ Authentication successful!")
      println(s"   Token: ${token.take(50)}...")
      println()
      
      // Test catalog search
      println("Testing catalog search (simplified query)...")
      val testResult = CopernicusOData.searchProductsSimple("31UCS", token)
      
      testResult match {
        case scala.util.Success(prods) =>
          if (prods.isEmpty) {
            println("⚠️  No products found with simple query")
            println("   This might indicate an API issue or tile name problem")
          } else {
            println(s"✅ Found ${prods.length} recent products for tile 31UCS:")
            prods.take(3).foreach { p =>
              println(s"   - ${p.Name}")
              println(s"     Date: ${p.ContentDate.Start}")
              println(s"     Size: ${p.ContentLength / 1_000_000} MB")
              println(s"     Online: ${p.Online}")
              println()
            }
          }
          
        case scala.util.Failure(e) =>
          println(s"❌ Catalog search failed: ${e.getMessage}")
      }
      
    case scala.util.Failure(ex) =>
      println(s"❌ Authentication failed: ${ex.getMessage}")
  }
}

