# Quick Start Guide

## 1. Install GDAL

**Windows**:
- Download from https://www.gisinternals.com/
- Install GDAL core components
- Default path: `C:\Program Files (x86)\GDAL`

**Linux**:
```bash
sudo apt install gdal-bin
```

**macOS**:
```bash
brew install gdal
```

## 2. Configure Credentials

```bash
cd cyfra-satellite
cp copernicus-credentials.template copernicus-credentials.properties
```

Edit `copernicus-credentials.properties`:
```properties
copernicus.username=your_email@example.com
copernicus.password=your_password
```

Register at: https://dataspace.copernicus.eu

## 3. Start the Server

**Windows**:
```powershell
.\run-server.ps1
```

**Linux/macOS**:
```bash
./run-server.sh
```

## 4. Open Browser

Navigate to: **http://localhost:8080**

## 5. Analyze Data

1. Select "Real Data" mode
2. Choose tile: `31UCS` (Paris)
3. Pick a date from available dates
4. Click "Analyze"
5. View results!

## First Run

The first analysis will:
- Download ~1GB product ZIP (~30-120 seconds)
- Extract bands (~5 seconds)
- Convert JP2 to TIFF with GDAL (~30-60 seconds)
- Compute indices on GPU (~50-100ms)

**Total**: 2-3 minutes

## Subsequent Runs

All files are cached:
- No download needed
- No conversion needed
- Direct TIFF loading

**Total**: < 1 second

## Troubleshooting

### "GDAL not found"
- Make sure GDAL is installed
- Check the path in `run-server.ps1` (Windows) or `run-server.sh` (Linux/macOS)
- Verify: `gdal_translate --version`

### "Failed to obtain OAuth token"
- Check `copernicus-credentials.properties` file exists
- Verify username and password are correct
- Ensure you registered at https://dataspace.copernicus.eu

### "No products found"
- Try a different date (within ±3 days)
- Try a different tile (e.g., `32TMT` for Rome)
- Check available dates in the web UI first

## More Info

- Full documentation: [README.md](README.md)
- GDAL setup: [GDAL_SETUP.md](GDAL_SETUP.md)
- Project details: [ACCOMPLISHMENTS.md](ACCOMPLISHMENTS.md)













