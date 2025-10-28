# Sentinel-2 Tile Finder Guide

## Quick Reference: How to Find Tiles

### Method 1: Copernicus Data Space Browser (Recommended)

1. **Visit**: https://dataspace.copernicus.eu/browser/
2. **Click** on any location on the map
3. **View** the tile ID in the results (format: `31UCS`)
4. **Check** available dates and cloud coverage
5. **Copy** the tile ID for use in Cyfra

### Method 2: Geographic Lookup

Use this approximate guide based on coordinates:

#### United States
- **East Coast** (NYC): `18TWL`
- **West Coast** (SF): `10SEG`, `10SFH`
- **California** (LA): `11SKA`, `11SLA`
- **Texas** (Houston): `15RTN`, `15RTP`
- **Colorado** (Denver): `13SED`, `13TDE`
- **Washington** (Seattle): `10TDK`, `10TDL`

#### Europe
- **UK** (London): `30UWU`, `30UXC`
- **France** (Paris): `31UCS`, `31UDR`
- **Germany** (Berlin): `32UPU`, `33UUU`
- **Spain** (Madrid): `30SVG`, `30TWL`
- **Italy** (Rome): `33TTG`, `33TUG`

#### Asia
- **Japan** (Tokyo): `54SUE`, `54SVE`
- **China** (Beijing): `50TMK`, `51RUQ`
- **India** (Delhi): `43RGQ`, `43RHP`
- **Thailand** (Bangkok): `47PPS`, `47PPT`

#### South America
- **Brazil** (São Paulo): `23KPQ`
- **Brazil** (Amazon): `20LPP`, `21LXF`, `21LYH`
- **Argentina** (Buenos Aires): `21HUB`, `21HVB`

#### Africa
- **Egypt** (Cairo): `36MZE`, `36RXV`
- **Kenya** (Nairobi): `36MZE`, `37MBU`
- **South Africa** (Cape Town): `34HDF`, `34HDG`

### Method 3: MGRS Calculator

If you have GPS coordinates:

1. **Visit**: https://www.mgrs-data.org/mgrs/
2. **Enter** latitude and longitude
3. **Get** MGRS/UTM tile ID

### Method 4: Using Sentinel Hub Playground

1. **Visit**: https://apps.sentinel-hub.com/eo-browser/
2. **Search** for location by name
3. **Zoom** to desired area
4. **Check** the tile info panel (shows tile ID)

## Understanding Tile Coverage

### Tile Size
- Each tile is approximately **110km × 110km** at the equator
- Size varies slightly at different latitudes
- Tiles overlap by a few kilometers

### Coverage
- **Global coverage**: All land areas
- **Coastal areas**: Includes ocean up to coastline
- **Polar regions**: Limited above 84°N and below 80°S

### Revisit Time
- **5 days** (with both Sentinel-2A and 2B)
- **10 days** (single satellite)
- More frequent near poles due to orbit overlap

## Finding Tiles for Specific Use Cases

### 🌳 Deforestation Monitoring
**Amazon Basin**:
- `20LPP`, `20LQQ`, `21LXF`, `21LYH`, `21LZG`
- `22MBT`, `22MCT`

**Indonesia**:
- `48MYU`, `49MGU`, `49MHU`
- `50MMB`, `50MNC`

### 🔥 Fire/Burn Detection
**California Fire Zones**:
- `10TFK`, `10TGK`, `11SKA`, `11SLA`
- `10SEG`, `10SFH` (Bay Area)

**Australia Bushfire Zones**:
- `55HFA`, `55HGB` (Melbourne/Victoria)
- `56HLH`, `56HMH` (Sydney/NSW)

### 🏙️ Urban Growth Monitoring
**Fastest Growing Cities**:
- Dubai: `40REM`, `40RFM`
- Shenzhen: `49QGE`, `49QGF`
- Lagos: `31NEH`, `31NFH`
- Delhi: `43RGQ`, `43RHP`

### 🌊 Water/Coastal Monitoring
**Major Deltas**:
- Nile Delta: `36MZE`, `36NVE`
- Ganges Delta: `45RVL`, `46QDJ`
- Mississippi Delta: `15RYP`, `16RCS`

**Coastal Erosion**:
- Bangladesh Coast: `45RVL`, `46QDJ`
- Louisiana Coast: `15RYP`, `15RYQ`
- Netherlands Coast: `31UFU`, `31UFS`

### 🏔️ Glaciers/Snow
**Major Glaciers**:
- Alps: `32TLS`, `32TLT`, `32TMS` (Switzerland/Austria)
- Himalayas: `44RKR`, `45RVL`, `45RWL`
- Iceland: `28WCU`, `28WDU`, `29WNV`

### 🌾 Agriculture
**Wheat Belts**:
- Ukraine: `36UXV`, `36UYA` (Kiev region)
- Kansas: `14SNG`, `14SNH` (US wheat belt)
- Australia: `54HVH`, `54HWH`

**Rice Paddies**:
- Vietnam: `48PXS`, `48PYS`
- Thailand: `47PPS`, `47PPT`
- India: `44QKG`, `45QWC`

## Pro Tips

### 1. **Check Cloud Coverage**
- Use Copernicus Browser to filter by cloud coverage
- Dry season typically has clearer images
- Consider season for your analysis

### 2. **Tile Boundaries**
- Some cities span multiple tiles
- Large areas may require mosaicking multiple tiles
- Use adjacent tiles for complete coverage

### 3. **Data Availability**
- Not all tiles have frequent updates
- Remote areas may have less frequent coverage
- Check available dates before planning analysis

### 4. **Resolution Considerations**
- RGB + NIR bands: 10m resolution
- Red Edge bands: 20m resolution
- SWIR bands: 20m resolution (needed for NBR)
- Atmospheric bands: 60m resolution

## Quick Lookup by Region

### North America
| Location | Tile ID | Notes |
|----------|---------|-------|
| New York | 18TWL | Manhattan and surrounding |
| Los Angeles | 11SKA | Central LA |
| San Francisco | 10SEG | Bay Area |
| Chicago | 16TDM | Lake Michigan shore |
| Miami | 17RNJ | Coastal Florida |
| Seattle | 10TDK | Puget Sound |

### Europe
| Location | Tile ID | Notes |
|----------|---------|-------|
| London | 30UWU | Thames River |
| Paris | 31UCS | Seine River |
| Berlin | 32UPU | City center |
| Madrid | 30SVG | Central Spain |
| Rome | 33TTG | Historic center |
| Amsterdam | 31UFU | Canal district |

### Asia
| Location | Tile ID | Notes |
|----------|---------|-------|
| Tokyo | 54SUE | Metropolitan area |
| Beijing | 50TMK | City center |
| Mumbai | 43PFL | Coastal city |
| Bangkok | 47PPS | Chao Phraya River |
| Singapore | 48NWG | Island nation |
| Dubai | 40REM | Desert coast |

### Environmental Interest
| Feature | Tile ID | Notes |
|---------|---------|-------|
| Amazon | 21LXF | Rainforest |
| Sahara | 32RMQ | Desert |
| Himalayas | 45RVL | Mountains |
| Great Barrier Reef | 55KDV | Coral reef |
| Iceland Glaciers | 28WDU | Ice/volcanic |

## Adding Tiles to Cyfra

Once you find a tile ID:

```scala
// Add to TileCatalog.scala
TileInfo("18TWL", "New York City", "USA", "North America", "Urban",
  "Dense urban metropolis, Manhattan island", 40.7128, -74.0060,
  List("urban", "metropolis", "dense", "city", "coastal"))
```

## Resources

1. **Copernicus Open Access Hub**: https://dataspace.copernicus.eu/
2. **Sentinel-2 User Guide**: https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi
3. **MGRS Information**: https://en.wikipedia.org/wiki/Military_Grid_Reference_System
4. **Tile Grid KML**: Available from ESA Sentinel portal

## Support

For questions about specific tiles or help finding locations:
- Copernicus Forum: https://forum.copernicus.eu/
- ESA Sentinel Portal: https://sentinels.copernicus.eu/


