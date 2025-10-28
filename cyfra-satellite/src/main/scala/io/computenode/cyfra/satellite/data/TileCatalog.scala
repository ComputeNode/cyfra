package io.computenode.cyfra.satellite.data

/** Comprehensive catalog of Sentinel-2 tiles worldwide
  * 
  * Each tile is approximately 110km x 110km at the equator
  * Tiles are identified by UTM zone, latitude band, and grid square
  */
object TileCatalog:
  
  case class TileInfo(
      id: String,
      name: String,
      country: String,
      region: String,
      category: String,
      description: String,
      latitude: Double,
      longitude: Double,
      keywords: List[String]
  )
  
  /** Full catalog of interesting and commonly used tiles */
  val catalog: List[TileInfo] = List(
    // === EUROPE ===
    // United Kingdom
    TileInfo("30UWU", "London", "United Kingdom", "Europe", "Urban", 
      "Major metropolitan area, River Thames", 51.5074, -0.1278,
      List("urban", "city", "capital", "europe", "thames", "metropolitan")),
    
    TileInfo("30UVB", "Manchester", "United Kingdom", "Europe", "Urban",
      "Industrial city in northern England", 53.4808, -2.2426,
      List("urban", "industrial", "city", "england")),
    
    // France
    TileInfo("31UCS", "Paris", "France", "Europe", "Urban",
      "Capital city with Seine River, mixed urban and vegetation", 48.8566, 2.3522,
      List("urban", "city", "capital", "europe", "seine", "cultural")),
    
    TileInfo("31TDJ", "Bordeaux", "France", "Europe", "Agriculture",
      "Wine region and coastal city", 44.8378, -0.5792,
      List("agriculture", "wine", "coastal", "vineyard")),
    
    TileInfo("31TCJ", "Lyon", "France", "Europe", "Urban",
      "Major city at confluence of Rhône and Saône", 45.7640, 4.8357,
      List("urban", "city", "river", "industrial")),
    
    // Germany
    TileInfo("32UPU", "Berlin", "Germany", "Europe", "Urban",
      "Capital city with lakes and forests", 52.5200, 13.4050,
      List("urban", "capital", "city", "europe")),
    
    TileInfo("32UMC", "Munich", "Germany", "Europe", "Urban",
      "Southern German city, gateway to Alps", 48.1351, 11.5820,
      List("urban", "city", "alps")),
    
    // Spain
    TileInfo("30SVG", "Madrid", "Spain", "Europe", "Urban",
      "Capital city in central Spain", 40.4168, -3.7038,
      List("urban", "capital", "city", "europe")),
    
    TileInfo("31TBF", "Barcelona", "Spain", "Europe", "Urban",
      "Coastal Mediterranean city", 41.3851, 2.1734,
      List("urban", "coastal", "mediterranean", "city")),
    
    // Italy
    TileInfo("33TTG", "Rome", "Italy", "Europe", "Urban",
      "Historic capital city", 41.9028, 12.4964,
      List("urban", "capital", "historic", "city")),
    
    TileInfo("32TQM", "Milan", "Italy", "Europe", "Urban",
      "Northern Italian industrial center", 45.4642, 9.1900,
      List("urban", "industrial", "city")),
    
    // Netherlands
    TileInfo("31UFU", "Amsterdam", "Netherlands", "Europe", "Urban",
      "Canal city and capital", 52.3676, 4.9041,
      List("urban", "capital", "canals", "water")),
    
    // Belgium
    TileInfo("31UES", "Brussels", "Belgium", "Europe", "Urban",
      "EU capital city", 50.8503, 4.3517,
      List("urban", "capital", "eu", "political")),
    
    // Switzerland
    TileInfo("32TMT", "Zurich", "Switzerland", "Europe", "Urban",
      "Financial center with lake", 47.3769, 8.5417,
      List("urban", "financial", "lake", "alps")),
    
    // Poland
    TileInfo("34UEB", "Warsaw", "Poland", "Europe", "Urban",
      "Capital city on Vistula River", 52.2297, 21.0122,
      List("urban", "capital", "river")),
    
    // Greece
    TileInfo("34SGE", "Athens", "Greece", "Europe", "Urban",
      "Historic capital and port city", 37.9838, 23.7275,
      List("urban", "capital", "historic", "coastal")),
    
    // === NORTH AMERICA ===
    // United States - East Coast
    TileInfo("18TWL", "New York City", "USA", "North America", "Urban",
      "Dense urban metropolis, Manhattan island", 40.7128, -74.0060,
      List("urban", "metropolis", "dense", "city", "coastal")),
    
    TileInfo("18SUJ", "Washington DC", "USA", "North America", "Urban",
      "US capital city", 38.9072, -77.0369,
      List("urban", "capital", "political", "city")),
    
    TileInfo("17SNV", "Philadelphia", "USA", "North America", "Urban",
      "Historic East Coast city", 39.9526, -75.1652,
      List("urban", "historic", "city")),
    
    TileInfo("18TVN", "Boston", "USA", "North America", "Urban",
      "Historic New England city", 42.3601, -71.0589,
      List("urban", "historic", "coastal", "city")),
    
    // United States - West Coast
    TileInfo("10SEG", "San Francisco", "USA", "North America", "Urban",
      "Bay Area tech hub", 37.7749, -122.4194,
      List("urban", "tech", "bay", "coastal", "city")),
    
    TileInfo("11SKA", "Los Angeles", "USA", "North America", "Urban",
      "Large Southern California metropolis", 34.0522, -118.2437,
      List("urban", "metropolis", "entertainment", "coastal")),
    
    TileInfo("10TDK", "Seattle", "USA", "North America", "Urban",
      "Pacific Northwest city", 47.6062, -122.3321,
      List("urban", "tech", "coastal", "pacific")),
    
    // United States - Other
    TileInfo("16SDE", "Denver", "USA", "North America", "Urban",
      "Mountain city and tech center", 39.7392, -104.9903,
      List("urban", "mountains", "tech")),
    
    TileInfo("16SEG", "Chicago", "USA", "North America", "Urban",
      "Great Lakes city, major transport hub", 41.8781, -87.6298,
      List("urban", "transport", "lake", "city")),
    
    TileInfo("15RTN", "Houston", "USA", "North America", "Urban",
      "Energy industry hub", 29.7604, -95.3698,
      List("urban", "energy", "industrial", "coastal")),
    
    TileInfo("17RPQ", "Miami", "USA", "North America", "Urban",
      "Coastal subtropical city", 25.7617, -80.1918,
      List("urban", "coastal", "subtropical", "tourism")),
    
    // Canada
    TileInfo("17TNJ", "Toronto", "Canada", "North America", "Urban",
      "Largest Canadian city on Lake Ontario", 43.6532, -79.3832,
      List("urban", "lake", "financial", "city")),
    
    TileInfo("10UEE", "Vancouver", "Canada", "North America", "Urban",
      "Pacific coastal city", 49.2827, -123.1207,
      List("urban", "coastal", "pacific", "mountains")),
    
    TileInfo("18TYR", "Montreal", "Canada", "North America", "Urban",
      "French-speaking Canadian city", 45.5017, -73.5673,
      List("urban", "cultural", "river", "city")),
    
    // Mexico
    TileInfo("14QNG", "Mexico City", "Mexico", "North America", "Urban",
      "Massive high-altitude capital", 19.4326, -99.1332,
      List("urban", "capital", "metropolis", "altitude")),
    
    // === SOUTH AMERICA ===
    TileInfo("23KPQ", "São Paulo", "Brazil", "South America", "Urban",
      "Largest city in South America", -23.5505, -46.6333,
      List("urban", "metropolis", "industrial", "city")),
    
    TileInfo("23KLR", "Rio de Janeiro", "Brazil", "South America", "Urban",
      "Coastal city with famous beaches", -22.9068, -43.1729,
      List("urban", "coastal", "tourism", "beaches")),
    
    TileInfo("21LXF", "Amazon Rainforest (Manaus)", "Brazil", "South America", "Forest",
      "Dense tropical rainforest near Manaus", -3.1190, -60.0217,
      List("rainforest", "tropical", "biodiversity", "amazon", "deforestation")),
    
    TileInfo("21LYH", "Amazon Rainforest (Santarem)", "Brazil", "South America", "Forest",
      "Amazon basin deforestation monitoring", -2.4411, -54.7082,
      List("rainforest", "deforestation", "amazon", "forest")),
    
    TileInfo("19HCC", "Buenos Aires", "Argentina", "South America", "Urban",
      "Argentine capital on Rio de la Plata", -34.6037, -58.3816,
      List("urban", "capital", "river", "city")),
    
    // === ASIA ===
    // China
    TileInfo("51RUQ", "Beijing", "China", "Asia", "Urban",
      "Chinese capital, major urban center", 39.9042, 116.4074,
      List("urban", "capital", "metropolis", "city")),
    
    TileInfo("51RTQ", "Shanghai", "China", "Asia", "Urban",
      "Massive coastal metropolis and port", 31.2304, 121.4737,
      List("urban", "coastal", "port", "metropolis", "financial")),
    
    TileInfo("49SGU", "Hong Kong", "China", "Asia", "Urban",
      "Dense urban area and financial hub", 22.3193, 114.1694,
      List("urban", "dense", "financial", "coastal")),
    
    // Japan
    TileInfo("54SUE", "Tokyo", "Japan", "Asia", "Urban",
      "World's largest metropolitan area", 35.6762, 139.6503,
      List("urban", "metropolis", "capital", "dense", "city")),
    
    TileInfo("53SNM", "Osaka", "Japan", "Asia", "Urban",
      "Major port city and industrial center", 34.6937, 135.5023,
      List("urban", "port", "industrial", "city")),
    
    // India
    TileInfo("43RGQ", "New Delhi", "India", "Asia", "Urban",
      "Indian capital", 28.6139, 77.2090,
      List("urban", "capital", "city")),
    
    TileInfo("43PFL", "Mumbai", "India", "Asia", "Urban",
      "Coastal megacity and financial center", 19.0760, 72.8777,
      List("urban", "coastal", "financial", "metropolis")),
    
    TileInfo("45RUL", "Kolkata", "India", "Asia", "Urban",
      "Major city on Ganges delta", 22.5726, 88.3639,
      List("urban", "delta", "river", "city")),
    
    // Southeast Asia
    TileInfo("47PPS", "Bangkok", "Thailand", "Asia", "Urban",
      "Thai capital with river and canals", 13.7563, 100.5018,
      List("urban", "capital", "river", "tropical")),
    
    TileInfo("48NWG", "Singapore", "Singapore", "Asia", "Urban",
      "Dense island city-state", 1.3521, 103.8198,
      List("urban", "island", "dense", "financial", "tropical")),
    
    TileInfo("48NWH", "Jakarta", "Indonesia", "Asia", "Urban",
      "Indonesian capital on Java", -6.2088, 106.8456,
      List("urban", "capital", "coastal", "tropical")),
    
    // Middle East
    TileInfo("38SMB", "Dubai", "UAE", "Asia", "Urban",
      "Modern desert metropolis", 25.2048, 55.2708,
      List("urban", "desert", "modern", "coastal")),
    
    TileInfo("36RYV", "Istanbul", "Turkey", "Asia", "Urban",
      "Historic city spanning Europe and Asia", 41.0082, 28.9784,
      List("urban", "historic", "coastal", "intercontinental")),
    
    // === AFRICA ===
    TileInfo("36MZE", "Cairo", "Egypt", "Africa", "Urban",
      "Capital city near Nile Delta", 30.0444, 31.2357,
      List("urban", "capital", "nile", "desert", "historic")),
    
    TileInfo("36KWD", "Nairobi", "Kenya", "Africa", "Urban",
      "East African capital", -1.2864, 36.8172,
      List("urban", "capital", "savanna")),
    
    TileInfo("35JMK", "Lagos", "Nigeria", "Africa", "Urban",
      "Massive coastal West African city", 6.5244, 3.3792,
      List("urban", "coastal", "metropolis", "tropical")),
    
    TileInfo("34HFH", "Johannesburg", "South Africa", "Africa", "Urban",
      "Largest South African city", -26.2041, 28.0473,
      List("urban", "mining", "industrial", "city")),
    
    TileInfo("34HDG", "Cape Town", "South Africa", "Africa", "Urban",
      "Coastal city with Table Mountain", -33.9249, 18.4241,
      List("urban", "coastal", "tourism", "mountain")),
    
    // === OCEANIA ===
    TileInfo("56HLH", "Sydney", "Australia", "Oceania", "Urban",
      "Major coastal city with harbor", -33.8688, 151.2093,
      List("urban", "coastal", "harbor", "city")),
    
    TileInfo("55HFA", "Melbourne", "Australia", "Oceania", "Urban",
      "Southern Australian cultural capital", -37.8136, 144.9631,
      List("urban", "cultural", "coastal", "city")),
    
    TileInfo("56JKT", "Brisbane", "Australia", "Oceania", "Urban",
      "Subtropical coastal city", -27.4698, 153.0251,
      List("urban", "subtropical", "coastal")),
    
    TileInfo("60KWE", "Auckland", "New Zealand", "Oceania", "Urban",
      "Largest New Zealand city", -36.8485, 174.7633,
      List("urban", "volcanic", "coastal", "harbor")),
    
    // === SPECIAL INTEREST ===
    // Deserts
    TileInfo("32RMQ", "Sahara Desert", "Algeria", "Africa", "Desert",
      "World's largest hot desert", 23.8859, 5.5281,
      List("desert", "arid", "sand", "sahara", "climate")),
    
    TileInfo("39RTM", "Rub al Khali", "Saudi Arabia", "Asia", "Desert",
      "Empty Quarter - vast sand desert", 20.5994, 51.4215,
      List("desert", "sand", "arid", "extreme")),
    
    // Ice & Snow
    TileInfo("19FDG", "Iceland (Reykjavik)", "Iceland", "Europe", "Volcanic",
      "Volcanic island with glaciers", 64.1466, -21.9426,
      List("volcanic", "glaciers", "geothermal", "arctic")),
    
    // Agriculture
    TileInfo("15TWK", "US Midwest (Iowa)", "USA", "North America", "Agriculture",
      "Intensive corn and soybean farming", 42.0046, -93.2140,
      List("agriculture", "crops", "farming", "corn", "soybean")),
    
    TileInfo("32UMV", "Ukraine (Kiev)", "Ukraine", "Europe", "Agriculture",
      "Major wheat production region", 50.4501, 30.5234,
      List("agriculture", "wheat", "farming", "breadbasket")),
    
    // Coastal & Islands
    TileInfo("22KCB", "Hawaii (Honolulu)", "USA", "Oceania", "Volcanic",
      "Volcanic Pacific islands", 21.3099, -157.8581,
      List("volcanic", "island", "tropical", "pacific", "tourism")),
    
    TileInfo("18LVQ", "Caribbean (Puerto Rico)", "USA", "North America", "Coastal",
      "Tropical island in Caribbean", 18.2208, -66.5901,
      List("tropical", "island", "caribbean", "coastal", "tourism")),
    
    // Mountains
    TileInfo("45RVL", "Himalayas (Nepal)", "Nepal", "Asia", "Mountain",
      "World's highest mountain range", 28.3949, 84.1240,
      List("mountains", "himalayas", "snow", "extreme", "altitude")),
    
    TileInfo("32TPS", "Alps (Switzerland)", "Switzerland", "Europe", "Mountain",
      "Major European mountain range", 46.5197, 8.3093,
      List("mountains", "alps", "snow", "glaciers")),
    
    // Disasters & Change Detection
    TileInfo("10SFH", "California (San Francisco Bay)", "USA", "North America", "Change",
      "Urban sprawl and fire monitoring", 37.8272, -122.2913,
      List("urban", "fire", "change", "monitoring", "bay")),
    
    TileInfo("54SVE", "Fukushima", "Japan", "Asia", "Change",
      "Post-disaster recovery monitoring", 37.4200, 140.4683,
      List("disaster", "recovery", "change", "monitoring")),
  )
  
  /** Search tiles by keyword, name, country, or category */
  def search(query: String): List[TileInfo] = {
    val lowerQuery = query.toLowerCase.trim
    if (lowerQuery.isEmpty) catalog
    else catalog.filter { tile =>
      tile.name.toLowerCase.contains(lowerQuery) ||
      tile.country.toLowerCase.contains(lowerQuery) ||
      tile.region.toLowerCase.contains(lowerQuery) ||
      tile.category.toLowerCase.contains(lowerQuery) ||
      tile.description.toLowerCase.contains(lowerQuery) ||
      tile.keywords.exists(_.contains(lowerQuery)) ||
      tile.id.toLowerCase.contains(lowerQuery)
    }
  }
  
  /** Filter by category */
  def byCategory(category: String): List[TileInfo] =
    catalog.filter(_.category.equalsIgnoreCase(category))
  
  /** Filter by region */
  def byRegion(region: String): List[TileInfo] =
    catalog.filter(_.region.equalsIgnoreCase(region))
  
  /** Filter by country */
  def byCountry(country: String): List[TileInfo] =
    catalog.filter(_.country.equalsIgnoreCase(country))
  
  /** Get all unique categories */
  def categories: List[String] =
    catalog.map(_.category).distinct.sorted
  
  /** Get all unique regions */
  def regions: List[String] =
    catalog.map(_.region).distinct.sorted
  
  /** Get all unique countries */
  def countries: List[String] =
    catalog.map(_.country).distinct.sorted
  
  /** Find tile by ID */
  def findById(id: String): Option[TileInfo] =
    catalog.find(_.id.equalsIgnoreCase(id))


