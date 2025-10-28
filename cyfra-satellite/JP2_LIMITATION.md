# JPEG2000 Reading Limitation

## Issue

Sentinel-2 L2A products use JPEG2000 (`.jp2`) format for spectral bands. The downloaded 10m resolution bands are ~110MB each, and none of the available pure Java JPEG2000 libraries can handle them:

1. **jai-imageio-jpeg2000 (1.4.0)**: Throws "File too long" error for files >100MB
2. **Apache Commons Imaging**: Doesn't recognize `.jp2` extension 
3. **TwelveMonkeys**: Doesn't include JPEG2000 support

## Tested Libraries

- `com.github.jai-imageio:jai-imageio-jpeg2000:1.4.0` ❌ Size limit exceeded
- `org.apache.commons:commons-imaging:1.0-alpha3` ❌ No JP2 support
- `com.twelvemonkeys.imageio:imageio-*:3.11.0` ❌ No JP2 plugin

## Workarounds

### 1. Use Native Libraries (Recommended for Production)

Install OpenJPEG or GDAL and convert files:

```bash
# Using GDAL
gdal_translate -of GTiff input.jp2 output.tif

# Using OpenJPEG
opj_decompress -i input.jp2 -o output.tif
```

### 2. Use Lower Resolution Bands

Download 60m resolution bands instead (much smaller files):

```scala
val bands = List("B02", "B03", "B04", "B08")  // at 60m resolution
```

### 3. External Processing Service

Run a preprocessing service that:
1. Downloads JP2 files
2. Converts to TIFF/PNG
3. Serves processed files

### 4. Use Sentinel-2 COG Products

Some providers offer Cloud-Optimized GeoTIFF (COG) versions of Sentinel-2 data which don't have this issue.

## Current Status

✅ **OAuth Authentication**: Working
✅ **Product Search (OData)**: Working  
✅ **Product Download**: Working (1GB+ ZIPs)  
✅ **ZIP Extraction**: Working  
✅ **Band File Location**: Working  
✅ **GPU Processing Pipeline**: Working (tested with synthetic data)  
✅ **Web UI**: Working  
❌ **JPEG2000 Reading**: Blocked by Java library limitations  

## Recommendation

For production use, implement workaround #1 or #3. The core Cyfra GPU pipeline is proven and working. The only missing piece is decoding the large JPEG2000 files.



