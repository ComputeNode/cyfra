# GDAL Setup for Sentinel-2 JP2 Processing

## Why GDAL is Needed

Sentinel-2 band files are distributed as JPEG2000 (JP2) files, which are typically 100-120 MB each. Pure Java libraries cannot handle files this large due to internal limitations. GDAL (Geospatial Data Abstraction Library) is the industry-standard tool for geospatial data conversion and can efficiently convert these files to TIFF format, which Java can read without issues.

## Installation

### Windows

1. **Download GDAL**:
   - Visit: https://www.gisinternals.com/
   - Select your platform (e.g., "release-1930-x64-gdal-3-8-4-mapserver-8-0-1.zip")
   - Download the "Generic installer for the GDAL core components"

2. **Install**:
   - Run the installer (e.g., `gdal-3.8.4-1930-x64-core.msi`)
   - Default installation path: `C:\Program Files\GDAL`

3. **Add to PATH**:
   ```powershell
   # Run as Administrator
   $gdalPath = "C:\Program Files\GDAL"
   [Environment]::SetEnvironmentVariable("Path", $env:Path + ";$gdalPath", [EnvironmentVariableTarget]::Machine)
   ```

4. **Verify Installation**:
   ```powershell
   # Open a NEW PowerShell window (to pick up PATH changes)
   gdal_translate --version
   ```
   
   Should output something like: `GDAL 3.8.4, released 2024/02/08`

### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install gdal-bin
gdal-translate --version
```

### macOS

```bash
brew install gdal
gdal-translate --version
```

## Running the Web Server with GDAL

### Easy Way (Recommended)

Use the provided launch scripts that automatically set up GDAL:

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

### Manual Way

Set the `GDAL_PATH` environment variable before running:

**Windows PowerShell**:
```powershell
$env:GDAL_PATH = "C:\Program Files (x86)\GDAL"
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"
```

**Linux/macOS**:
```bash
export GDAL_PATH="/usr/bin"
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"
```

### Auto-Detection

The system will automatically try these locations if `GDAL_PATH` is not set:
- Windows: `C:\Program Files (x86)\GDAL`, `C:\Program Files\GDAL`
- Linux/macOS: `/usr/bin`, `/usr/local/bin`

## How It Works

The `RealDataLoader` automatically:

1. **Detects JP2 files** when loading Sentinel-2 bands
2. **Checks for existing TIFF** conversion (cached)
3. **Runs GDAL conversion** if TIFF doesn't exist:
   ```
   gdal_translate -of GTiff -co COMPRESS=LZW -co TILED=YES input.jp2 output.tif
   ```
4. **Loads the TIFF** using standard Java ImageIO

### Performance

- **First run**: Converts all 4 bands (~30-60 seconds total)
- **Subsequent runs**: Uses cached TIFFs (< 1 second per band)

The converted TIFF files are stored alongside the original JP2 files and are automatically reused.

## Troubleshooting

### "gdal_translate: command not found"

**Windows**: 
- Restart your terminal/PowerShell after installation
- Verify PATH was updated: `echo $env:Path`
- Manually add GDAL to PATH (see installation steps above)

**Linux/macOS**:
- Run `which gdal_translate` to verify installation
- May need to restart terminal

### "File too long" error

This means GDAL is not installed, and the system fell back to the Java JP2 reader, which cannot handle large files. Install GDAL as described above.

### Conversion is slow

This is normal for the first run. JPEG2000 decompression is computationally intensive. Subsequent runs will be fast because the TIFFs are cached.

## Manual Conversion (Optional)

If you prefer to convert files manually before running the application:

```bash
cd satellite_data_test  # or your data directory

# Convert a single band
gdal_translate -of GTiff -co COMPRESS=LZW 31UCS_2025-10-24_B02_10m.jp2 31UCS_2025-10-24_B02_10m.tif

# Convert all JP2 files in directory (bash)
for f in *.jp2; do 
  gdal_translate -of GTiff -co COMPRESS=LZW "$f" "${f%.jp2}.tif"
done

# Convert all JP2 files in directory (PowerShell)
Get-ChildItem *.jp2 | ForEach-Object {
  $outFile = $_.FullName -replace '\.jp2$', '.tif'
  gdal_translate -of GTiff -co COMPRESS=LZW $_.FullName $outFile
}
```

## File Sizes

| Format | Typical Size | Notes |
|--------|-------------|-------|
| JP2 (original) | 110-120 MB | Compressed, cannot be read by Java |
| TIFF (LZW compressed) | 150-180 MB | Lossless, readable by Java |
| TIFF (uncompressed) | 200-220 MB | Faster but larger |

Using LZW compression (`-co COMPRESS=LZW`) provides a good balance between file size and read performance.

## Alternative: Direct libvips Integration

For production environments, consider using libvips with OpenJPEG support, which can read JP2 files directly without conversion. This requires native library installation but provides better performance. See the vips-ffm documentation for details.

