package io.computenode.cyfra.satellite.data

import io.circe.parser.*
import io.circe.generic.auto.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Instant
import scala.util.{Try, Success, Failure}

/** OAuth authentication for Copernicus Data Space Ecosystem
  *
  * Register at: https://dataspace.copernicus.eu/
  * Get credentials from: https://identity.dataspace.copernicus.eu/auth/realms/CDSE/account/
  */
object CopernicusAuth:

  private val TOKEN_URL = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
  
  // Cache for access token
  @volatile private var cachedToken: Option[(String, Instant)] = None
  
  case class TokenResponse(
      access_token: String,
      expires_in: Int,
      refresh_expires_in: Int,
      token_type: String
  )

  /** Get access token using username/password or client credentials
    *
    * @param clientId Optional client ID (default: "cdse-public")
    * @param clientSecret Optional client secret (for client_credentials flow)
    * @param username Optional username (for password flow)
    * @param password Optional password (for password flow)
    * @return Access token
    */
  def getAccessToken(
      clientId: Option[String] = None,
      clientSecret: Option[String] = None,
      username: Option[String] = None,
      password: Option[String] = None
  ): Try[String] = {
    // Check cache first (with 5-minute buffer before expiry)
    cachedToken match {
      case Some((token, expiry)) if Instant.now().plusSeconds(300).isBefore(expiry) =>
        println(s"  Using cached OAuth token (expires in ${java.time.Duration.between(Instant.now(), expiry).toMinutes} minutes)")
        return Success(token)
      case _ => // Token expired or not cached, fetch new one
    }

    Try {
      val httpClient = HttpClient.newHttpClient()
      
      // Construct request body based on available credentials
      val requestBody = (username, password, clientId, clientSecret) match {
        case (Some(user), Some(pass), _, _) =>
          // Username/password flow (recommended for Copernicus)
          val cid = clientId.getOrElse("cdse-public")
          s"grant_type=password&username=$user&password=$pass&client_id=$cid"
        case (_, _, Some(id), Some(secret)) =>
          // Client credentials flow
          s"grant_type=client_credentials&client_id=$id&client_secret=$secret"
        case _ =>
          throw new IllegalArgumentException(
            "Copernicus Data Space requires authentication.\n" +
            "Please provide EITHER:\n" +
            "  - username and password (easier), OR\n" +
            "  - client_id and client_secret (advanced)\n\n" +
            "Register at https://dataspace.copernicus.eu/"
          )
      }
      
      println(s"  Requesting OAuth token from Copernicus...")
      
      val request = HttpRequest.newBuilder()
        .uri(URI.create(TOKEN_URL))
        .header("Content-Type", "application/x-www-form-urlencoded")
        .POST(HttpRequest.BodyPublishers.ofString(requestBody))
        .build()
      
      val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
      
      if (response.statusCode() == 200) {
        // Parse JSON response
        decode[TokenResponse](response.body()) match {
          case Right(tokenResp) =>
            val expiry = Instant.now().plusSeconds(tokenResp.expires_in.toLong)
            cachedToken = Some((tokenResp.access_token, expiry))
            println(s"  ✓ OAuth token obtained (expires in ${tokenResp.expires_in / 60} minutes)")
            tokenResp.access_token
          case Left(error) =>
            throw new RuntimeException(s"Failed to parse token response: ${error.getMessage}")
        }
      } else {
        throw new RuntimeException(
          s"OAuth authentication failed: HTTP ${response.statusCode()}\n" +
          s"Response: ${response.body()}\n\n" +
          s"Please check your credentials at https://dataspace.copernicus.eu/"
        )
      }
    }
  }

  /** Get credentials from config file or environment variables
    *
    * Priority:
    * 1. copernicus-credentials.properties file
    * 2. Environment variables
    *
    * @return (username, password, clientId, clientSecret)
    */
  def getCredentialsFromEnv(): (Option[String], Option[String], Option[String], Option[String]) = {
    // Try loading from properties file first
    val configFile = java.nio.file.Paths.get("cyfra-satellite/copernicus-credentials.properties")
    val alternateConfigFile = java.nio.file.Paths.get("copernicus-credentials.properties")
    
    val fromFile = if (java.nio.file.Files.exists(configFile)) {
      loadCredentialsFromFile(configFile)
    } else if (java.nio.file.Files.exists(alternateConfigFile)) {
      loadCredentialsFromFile(alternateConfigFile)
    } else {
      (None, None, None, None)
    }
    
    // Fall back to environment variables if file not found
    if (fromFile._1.isDefined || fromFile._3.isDefined) {
      println("  ✓ Found Copernicus credentials in config file")
      fromFile
    } else {
      val envUser = sys.env.get("COPERNICUS_USERNAME")
      val envPass = sys.env.get("COPERNICUS_PASSWORD")
      val envId = sys.env.get("COPERNICUS_CLIENT_ID")
      val envSecret = sys.env.get("COPERNICUS_CLIENT_SECRET")
      
      if (envUser.isDefined && envPass.isDefined || envId.isDefined && envSecret.isDefined) {
        println("  ✓ Found Copernicus credentials in environment variables")
        (envUser, envPass, envId, envSecret)
      } else {
        (None, None, None, None)
      }
    }
  }
  
  /** Load credentials from properties file
    *
    * @param path Path to properties file
    * @return (username, password, client_id, client_secret)
    */
  private def loadCredentialsFromFile(path: java.nio.file.Path): (Option[String], Option[String], Option[String], Option[String]) = {
    Try {
      val props = new java.util.Properties()
      val input = new java.io.FileInputStream(path.toFile)
      try {
        props.load(input)
        val username = Option(props.getProperty("copernicus.username")).filter(_.nonEmpty).filter(_ != "YOUR_USERNAME_HERE")
        val password = Option(props.getProperty("copernicus.password")).filter(_.nonEmpty).filter(_ != "YOUR_PASSWORD_HERE")
        val clientId = Option(props.getProperty("copernicus.client.id")).filter(_.nonEmpty).filter(_ != "YOUR_CLIENT_ID_HERE").filter(_.length < 100) // Filter out JWT tokens
        val clientSecret = Option(props.getProperty("copernicus.client.secret")).filter(_.nonEmpty).filter(_ != "YOUR_CLIENT_SECRET_HERE").filter(_.length < 100)
        (username, password, clientId, clientSecret)
      } finally {
        input.close()
      }
    }.getOrElse((None, None, None, None))
  }
