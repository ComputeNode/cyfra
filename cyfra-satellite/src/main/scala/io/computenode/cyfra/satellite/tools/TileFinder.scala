package io.computenode.cyfra.satellite.tools

/** Helper tool to find Sentinel-2 tile IDs from coordinates
  * 
  * Uses approximate MGRS/UTM calculation
  * For precise results, use official Copernicus Browser
  */
object TileFinder:
  
  /** Approximate UTM zone from longitude */
  def getUTMZone(longitude: Double): Int = {
    // Special cases for Norway and Svalbard
    // For simplicity, using standard formula
    ((longitude + 180.0) / 6.0).floor.toInt + 1
  }
  
  /** Get latitude band letter from latitude */
  def getLatitudeBand(latitude: Double): Char = {
    // MGRS uses letters C-X (skipping I and O)
    // From 80°S to 84°N
    val bands = "CDEFGHJKLMNPQRSTUVWX"
    val index = ((latitude + 80.0) / 8.0).floor.toInt
    
    if (index < 0) 'C'
    else if (index >= bands.length) 'X'
    else bands.charAt(index)
  }
  
  /** Approximate tile ID from coordinates
    * 
    * Note: This gives an approximation. Grid square calculation
    * is complex and requires full MGRS library. Use Copernicus
    * Browser for exact tile IDs.
    * 
    * @param latitude Decimal degrees (-80 to 84)
    * @param longitude Decimal degrees (-180 to 180)
    * @return Approximate tile prefix (zone + band)
    */
  def approximateTilePrefix(latitude: Double, longitude: Double): String = {
    val zone = getUTMZone(longitude)
    val band = getLatitudeBand(latitude)
    f"$zone%02d$band"
  }
  
  /** Find the nearest known tile from catalog */
  def findNearestTile(latitude: Double, longitude: Double): Option[io.computenode.cyfra.satellite.data.TileCatalog.TileInfo] = {
    import io.computenode.cyfra.satellite.data.TileCatalog
    
    // Calculate distance to each tile
    val tilesWithDistance = TileCatalog.catalog.map { tile =>
      val distance = haversineDistance(latitude, longitude, tile.latitude, tile.longitude)
      (tile, distance)
    }
    
    // Return closest tile
    tilesWithDistance.minByOption(_._2).map(_._1)
  }
  
  /** Calculate great circle distance between two points (in km) */
  private def haversineDistance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val R = 6371.0 // Earth radius in km
    val dLat = math.toRadians(lat2 - lat1)
    val dLon = math.toRadians(lon2 - lon1)
    
    val a = math.sin(dLat / 2) * math.sin(dLat / 2) +
            math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) *
            math.sin(dLon / 2) * math.sin(dLon / 2)
    
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    R * c
  }
  
  /** Search tiles by location name or description */
  def searchByLocation(query: String): List[io.computenode.cyfra.satellite.data.TileCatalog.TileInfo] = {
    import io.computenode.cyfra.satellite.data.TileCatalog
    TileCatalog.search(query)
  }
  
  /** Interactive tile finder demo */
  def main(args: Array[String]): Unit = {
    println("=== Sentinel-2 Tile Finder ===\n")
    
    // Example locations
    val locations = List(
      ("New York City", 40.7128, -74.0060),
      ("London", 51.5074, -0.1278),
      ("Tokyo", 35.6762, 139.6503),
      ("Sydney", -33.8688, 151.2093),
      ("São Paulo", -23.5505, -46.6333),
      ("Cairo", 30.0444, 31.2357),
      ("Mumbai", 19.0760, 72.8777),
      ("Mount Everest", 27.9881, 86.9250),
      ("Amazon Rainforest", -3.4653, -62.2159),
      ("Sahara Desert", 23.8859, 5.5281)
    )
    
    println("Finding tiles for major locations:\n")
    
    locations.foreach { case (name, lat, lon) =>
      val prefix = approximateTilePrefix(lat, lon)
      val nearest = findNearestTile(lat, lon)
      
      println(s"📍 $name ($lat°, $lon°)")
      println(s"   Approximate tile prefix: $prefix")
      nearest.foreach { tile =>
        val distance = haversineDistance(lat, lon, tile.latitude, tile.longitude)
        println(f"   Nearest catalog tile: ${tile.id} - ${tile.name}")
        println(f"   Distance: $distance%.1f km")
      }
      println()
    }
    
    println("\n--- Searching by Keyword ---\n")
    
    val searchTerms = List("amazon", "desert", "fire", "urban")
    
    searchTerms.foreach { term =>
      val results = searchByLocation(term)
      println(s"Search: '$term' - Found ${results.length} tiles")
      results.take(3).foreach { tile =>
        println(s"  • ${tile.id} - ${tile.name}, ${tile.country}")
      }
      println()
    }
    
    println("\n=== How to Use ===")
    println("1. Use Copernicus Browser: https://dataspace.copernicus.eu/browser/")
    println("2. Click on your location of interest")
    println("3. Note the tile ID in the format: 31UCS")
    println("4. Add to TileCatalog.scala or use directly in your analysis")
    println("\nFor coordinate lookup: https://www.mgrs-data.org/mgrs/")
  }


