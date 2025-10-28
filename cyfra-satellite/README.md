# Cyfra Satellite - GPU-Accelerated Satellite Image Analysis

A GPU-powered satellite image analysis system built with Cyfra, featuring real Sentinel-2 data loading and spectral index computation.

## Features

✅ **Real Sentinel-2 Data** - Downloads from Copernicus Data Space Ecosystem  
✅ **GPU Compute** - All spectral indices computed on GPU via Cyfra  
✅ **OAuth Authentication** - Automated token management  
✅ **Web Interface** - Interactive analysis with date selection  
✅ **Automatic GDAL Conversion** - JP2 to TIFF for compatibility  
✅ **Caching** - Downloaded products and converted files cached locally  

## Quick Start

### 1. Prerequisites

#### Required:
- **Java 11+** (tested with Java 17)
- **Vulkan-capable GPU** (for Cyfra compute)
- **GDAL** (for JP2 file conversion) - [See GDAL Setup Guide](GDAL_SETUP.md)

#### Optional:
- Copernicus account for real data (free at https://dataspace.copernicus.eu)

### 2. Install GDAL

**Windows**:
```powershell
# Download from https://www.gisinternals.com/
# Install GDAL core components
# Add to PATH (see GDAL_SETUP.md for details)
```

**Linux**:
```bash
sudo apt install gdal-bin
```

**macOS**:
```bash
brew install gdal
```

Verify installation:
```bash
gdal_translate --version
```

### 3. Configure Copernicus Credentials

Copy the template:
```bash
cp copernicus-credentials.template copernicus-credentials.properties
```

Edit `copernicus-credentials.properties` with your credentials:
```properties
copernicus.username=your_email@example.com
copernicus.password=your_password
```

**Get credentials**: Register at https://dataspace.copernicus.eu

### 4. Run the Web Server

**Windows**:
```powershell
cd cyfra-satellite
.\run-server.ps1
```

**Linux/macOS**:
```bash
cd cyfra-satellite
./run-server.sh
```

**Alternative** (manual):
```bash
# Set GDAL path
export GDAL_PATH="C:\Program Files (x86)\GDAL"  # Windows
# or
export GDAL_PATH="/usr/bin"  # Linux/macOS

# Run server
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"
```

Open: http://localhost:8080

### 5. Analyze Satellite Data

1. **Select Mode**: Real Data or Synthetic
2. **Choose Tile**: e.g., `31UCS` (Paris)
3. **Pick Date**: Available dates shown for real data
4. **Click Analyze**: GPU computes all spectral indices
5. **View Results**: NDVI, EVI, NDWI, SAVI visualizations

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Web UI (JS)                          │
└────────────────────┬────────────────────────────────────────┘
                     │ HTTP API
┌────────────────────▼────────────────────────────────────────┐
│              SatelliteWebServer (Scala)                     │
└────┬────────────────────────────────────┬───────────────────┘
     │                                    │
     │ Load Data                          │ Compute Indices
     ▼                                    ▼
┌─────────────────┐              ┌────────────────────────────┐
│ RealDataLoader  │              │  SpectralIndices (Cyfra)  │
│  - OAuth Auth   │              │   - NDVI, EVI, NDWI, ...  │
│  - OData API    │              │   - GPU Compute Kernels   │
│  - ZIP Extract  │              │   - Vulkan Execution      │
│  - GDAL Convert │              └────────────────────────────┘
└─────────────────┘
```

### Data Flow

1. **Download** (if not cached):
   - Search Copernicus OData API for products
   - Download 1GB+ ZIP files
   - Extract band files (JP2)

2. **Convert** (automatic):
   - Detect JP2 files
   - Run GDAL conversion to TIFF
   - Cache converted files

3. **Load**:
   - Read TIFF with Java ImageIO
   - Subsample if needed (max 2048x2048)
   - Convert to float arrays

4. **Compute** (GPU):
   - Upload band data to GPU buffers
   - Execute Cyfra compute shaders
   - Calculate all spectral indices in parallel
   - Download results

5. **Visualize**:
   - Encode as PNG
   - Serve via HTTP
   - Display in browser

## Spectral Indices

All computed on GPU using Cyfra DSL:

- **NDVI** - Normalized Difference Vegetation Index
- **EVI** - Enhanced Vegetation Index  
- **NDWI** - Normalized Difference Water Index
- **SAVI** - Soil-Adjusted Vegetation Index
- **NDMI** - Normalized Difference Moisture Index
- **NBR** - Normalized Burn Ratio
- **RGB** - True color composite (B04, B03, B02)

## Example: Paris (31UCS)

```scala
val scene = RealDataLoader.loadSentinel2Scene(
  tile = TileId("31UCS"),
  date = LocalDate.of(2024, 8, 15)
)

val result = SpectralIndices.computeAll(scene, tileSize = 2048)

ImageUtils.savePNG(result.ndvi, "paris_ndvi.png")
```

## Project Structure

```
cyfra-satellite/
├── src/main/scala/
│   ├── data/
│   │   ├── RealDataLoader.scala      # Copernicus API integration
│   │   ├── CopernicusAuth.scala      # OAuth token management
│   │   ├── CopernicusOData.scala     # OData search/download
│   │   └── SatelliteImage.scala      # Data structures
│   ├── spectral/
│   │   └── SpectralIndices.scala     # GPU compute kernels (Cyfra)
│   ├── web/
│   │   └── SatelliteWebServer.scala  # HTTP server
│   └── examples/
│       └── QuickTest.scala            # Standalone examples
├── static/
│   ├── index.html                     # Web UI
│   ├── app.js                         # Frontend logic
│   └── style.css                      # Styling
├── copernicus-credentials.template    # Credentials template
├── GDAL_SETUP.md                      # GDAL installation guide
└── README.md                          # This file
```

## Troubleshooting

### "GDAL not found" error

GDAL is required to convert JP2 files. See [GDAL_SETUP.md](GDAL_SETUP.md) for installation instructions.

### "Failed to obtain OAuth token"

Check your `copernicus-credentials.properties` file:
- Username should be your email
- Password should be your Copernicus account password
- File must be in `cyfra-satellite/` directory

### "No products found for date"

Not all tiles have data on all dates. Try:
- A different date (within ±3 days)
- A different tile (e.g., `31UCS` for Paris, `32TMT` for Rome)
- Check available dates in the web UI

### "Out of memory" error

Reduce the tile size in `SpectralIndices.computeAll`:
```scala
val result = SpectralIndices.computeAll(scene, tileSize = 1024) // instead of 2048
```

### Web UI shows blank images

Check browser console for errors. Make sure the analysis completed successfully on the backend.

## Performance

### Real Data Loading (First Time)
- **Product search**: ~2 seconds
- **Download 1GB ZIP**: ~30-120 seconds (depends on connection)
- **Extract bands**: ~5 seconds
- **GDAL conversion**: ~30-60 seconds (4 bands)
- **Total**: ~2-3 minutes

### Subsequent Loads (Cached)
- **Total**: < 1 second (ZIP and TIFFs cached)

### GPU Computation
- **4 bands → 7 indices**: ~50-100ms on modern GPU
- **Tile size 2048x2048**: ~10M pixels processed

## Development

### Run Tests
```bash
sbt "project satellite" "runMain io.computenode.cyfra.satellite.examples.testRealDownload"
```

### Run Web Server
```bash
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"
```

### Clean Cache
```bash
rm -rf satellite_cache/  # Downloaded ZIPs and extracted bands
rm -rf satellite_data/    # Converted TIFFs
```

## Technology Stack

- **Cyfra** - GPU compute DSL (Scala → SPIR-V → Vulkan)
- **http4s** - HTTP server
- **Circe** - JSON processing
- **ImageIO** - TIFF reading
- **GDAL** - JP2 to TIFF conversion
- **Copernicus Data Space** - Sentinel-2 data source

## License

See main project LICENSE file.

## Acknowledgments

- **ESA Copernicus Programme** - For providing free Sentinel-2 data
- **GDAL Project** - For geospatial data conversion tools
- **Cyfra** - For GPU compute framework
