# Copernicus Data Space Ecosystem Setup Guide

## 🎯 Quick Summary

Real Sentinel-2 data download now works with **OAuth authentication**!

## 📋 Prerequisites

You need a free account at Copernicus Data Space Ecosystem.

## 🚀 Setup Instructions

### Step 1: Register (5 minutes)

1. Go to: **https://dataspace.copernicus.eu/**
2. Click **"Register"**
3. Fill in the registration form
4. Verify your email address
5. Log in to your account

### Step 2: Get OAuth Credentials (2 minutes)

1. Go to: **https://identity.dataspace.copernicus.eu/auth/realms/CDSE/account/**
2. Log in with your Copernicus credentials
3. Click on **"Applications"** or **"Clients"**
4. Create a new client or use the default one
5. Note down your:
   - `client_id` (looks like: `user-123456`)
   - `client_secret` (looks like: `abcd1234-5678-90ef-ghij-klmnopqrstuv`)

### Step 3: Set Environment Variables

**Windows (PowerShell):**
```powershell
$env:COPERNICUS_CLIENT_ID="your_client_id_here"
$env:COPERNICUS_CLIENT_SECRET="your_client_secret_here"
```

**Or set permanently:**
```powershell
[System.Environment]::SetEnvironmentVariable("COPERNICUS_CLIENT_ID", "your_client_id", "User")
[System.Environment]::SetEnvironmentVariable("COPERNICUS_CLIENT_SECRET", "your_secret", "User")
```

**Linux/Mac:**
```bash
export COPERNICUS_CLIENT_ID="your_client_id_here"
export COPERNICUS_CLIENT_SECRET="your_client_secret_here"
```

**Or add to `~/.bashrc` or `~/.zshrc`:**
```bash
echo 'export COPERNICUS_CLIENT_ID="your_client_id"' >> ~/.bashrc
echo 'export COPERNICUS_CLIENT_SECRET="your_secret"' >> ~/.bashrc
source ~/.bashrc
```

### Step 4: Test the Connection

```bash
sbt "project satellite" "runMain io.computenode.cyfra.satellite.examples.testRealData"
```

**Expected Output:**
```
📡 Testing Sentinel-2 Data Loading
   Tile: 31UCS
   Date: 2024-08-15

⬇️  Downloading Sentinel-2 bands from AWS S3...

  Found Copernicus credentials in environment variables
  Requesting OAuth token from Copernicus...
  ✓ OAuth token obtained (expires in 10 minutes)
  Searching Copernicus catalog for tile 31UCS on 2024-08-15...
  ✓ Found 2 product(s)
     - S2A_MSIL2A_20240815T103631_N0511_R008_T31UCS_20240815T152352.SAFE (1023 MB, Online: true)
  Using product: S2A_MSIL2A_20240815T103631_N0511_R008_T31UCS_20240815T152352.SAFE
  ⚠️  WARNING: Downloading full product (~1GB). Band extraction not yet implemented.
  ...
```

## 🎨 What Works Now

### ✅ OAuth Authentication
- Token retrieval
- Token caching (10-minute validity)
- Automatic refresh
- Environment variable loading

### ✅ OData API Integration
- Product search by tile + date
- Product metadata retrieval
- Collection filtering (Sentinel-2 L2A only)
- Date range queries

### ✅ Authenticated Downloads
- Direct download with Bearer token
- File size reporting
- Progress indication

### ⚠️ In Progress
- **Band extraction**: Currently downloads full product (~1GB)
- **Zipper API**: Need to integrate for selective file extraction
- **Streaming downloads**: For large files

## 📊 How It Works

```
┌─────────────────┐
│   Your Code     │
└────────┬────────┘
         │
         │ 1. Request download
         ▼
┌─────────────────┐
│ RealDataLoader  │  Check cache
└────────┬────────┘
         │
         │ 2. Get OAuth token
         ▼
┌─────────────────┐
│ CopernicusAuth  │  Request token from
│                 │  identity.dataspace.copernicus.eu
└────────┬────────┘
         │
         │ 3. Token cached
         ▼
┌─────────────────┐
│ CopernicusOData │  Search products via
│                 │  catalogue.dataspace.copernicus.eu
└────────┬────────┘
         │
         │ 4. Products found
         ▼
┌─────────────────┐
│ HTTP Download   │  Download with Authorization: Bearer
│                 │  From: zipper.dataspace.copernicus.eu
└────────┬────────┘
         │
         │ 5. File saved
         ▼
┌─────────────────┐
│ GPU Processing  │  NDVI, EVI, NDWI calculation
└─────────────────┘
```

## 🔐 Security Notes

- **Never commit credentials to Git!**
- Use environment variables only
- Tokens expire after 10 minutes (auto-renewed)
- OAuth uses client credentials flow (no user passwords)

## 🐛 Troubleshooting

### "Copernicus Data Space Authentication Required"

**Problem**: Environment variables not set

**Solution**: 
```powershell
# Check if variables are set
$env:COPERNICUS_CLIENT_ID
$env:COPERNICUS_CLIENT_SECRET

# If empty, set them as shown in Step 3
```

### "OAuth authentication failed: HTTP 401"

**Problem**: Invalid credentials

**Solutions**:
1. Double-check your client_id and client_secret
2. Make sure there are no extra spaces
3. Try creating a new client in Copernicus account settings
4. Verify your Copernicus account is active

### "No Sentinel-2 products found"

**Problem**: No data available for that tile/date

**Solutions**:
1. Try a different date
2. Check https://browser.dataspace.copernicus.eu/ for available dates
3. Use a more recent date (last 6 months have better coverage)
4. Try a different tile

### "WARNING: Downloading full product (~1GB)"

**Status**: This is expected behavior for now

**Workaround**:
- Use synthetic data mode for testing
- Wait for Zipper API integration (extracts only needed bands)
- Manually download bands from browser

## 🎯 Next Steps

### For Development (Use Synthetic Data)
```bash
sbt "project satellite" "runMain io.computenode.cyfra.satellite.examples.quickTest"
```
- ✅ Works without authentication
- ✅ Tests full GPU pipeline
- ✅ Generates beautiful visualizations

### For Production (Real Data with OAuth)
1. Set up credentials (Steps 1-3 above)
2. Test with one tile/date
3. Implement band extraction (Zipper API)
4. Set up automated workflows

## 📚 Additional Resources

- **Copernicus Portal**: https://dataspace.copernicus.eu/
- **API Documentation**: https://documentation.dataspace.copernicus.eu/
- **OData API Guide**: https://documentation.dataspace.copernicus.eu/APIs/OData.html
- **Browser (find dates)**: https://browser.dataspace.copernicus.eu/
- **Support Forum**: https://forum.dataspace.copernicus.eu/

## ✅ Success Criteria

You've successfully set up Copernicus access when you see:

```
✓ OAuth token obtained (expires in 10 minutes)
✓ Found 2 product(s)
```

---

**The OAuth implementation is complete! The GPU pipeline is ready for real data.** 🚀



