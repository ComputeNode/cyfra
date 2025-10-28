# Quick Credentials Setup

## Option 1: Configuration File (Recommended ⭐)

### Step 1: Create Config File
```bash
# Copy the template
cp cyfra-satellite/copernicus-credentials.template cyfra-satellite/copernicus-credentials.properties
```

Or on Windows:
```powershell
Copy-Item cyfra-satellite\copernicus-credentials.template cyfra-satellite\copernicus-credentials.properties
```

### Step 2: Get Your Credentials

1. **Register** at https://dataspace.copernicus.eu/ (free, takes 2 minutes)
2. **Log in** and go to: https://identity.dataspace.copernicus.eu/auth/realms/CDSE/account/
3. **Create/view** your OAuth client credentials
4. Copy your `client_id` and `client_secret`

### Step 3: Edit the File

Open `cyfra-satellite/copernicus-credentials.properties` and replace:

```properties
copernicus.client.id=YOUR_CLIENT_ID_HERE
copernicus.client.secret=YOUR_CLIENT_SECRET_HERE
```

With your actual credentials:

```properties
copernicus.client.id=user-123456
copernicus.client.secret=abcd1234-5678-90ef-ghij-klmnopqrstuv
```

### Step 4: Test It!

```bash
sbt "project satellite" "runMain io.computenode.cyfra.satellite.examples.testRealData"
```

**Expected Output:**
```
✓ Found Copernicus credentials in config file
✓ OAuth token obtained (expires in 10 minutes)
✓ Found 2 product(s)
```

---

## Option 2: Environment Variables

If you prefer environment variables instead:

### Windows (PowerShell):
```powershell
$env:COPERNICUS_CLIENT_ID="your_client_id"
$env:COPERNICUS_CLIENT_SECRET="your_secret"
```

### Linux/Mac:
```bash
export COPERNICUS_CLIENT_ID="your_client_id"
export COPERNICUS_CLIENT_SECRET="your_secret"
```

---

## ⚠️ Security Note

**NEVER commit `copernicus-credentials.properties` to Git!**

The file is already in `.gitignore` to prevent accidental commits.

---

## Priority Order

The system checks credentials in this order:

1. ✅ `cyfra-satellite/copernicus-credentials.properties`
2. ✅ `copernicus-credentials.properties` (in project root)
3. ✅ Environment variables

Use whichever method is most convenient for you!

---

## Troubleshooting

### "No credentials found"

**Check:**
```bash
# Does the file exist?
ls cyfra-satellite/copernicus-credentials.properties

# Is it properly formatted?
cat cyfra-satellite/copernicus-credentials.properties
```

### "OAuth authentication failed"

**Problem:** Invalid credentials

**Solution:** 
1. Double-check you copied the full client_id and client_secret
2. Make sure there are no extra spaces
3. Try generating new credentials in Copernicus account

### "File not found"

**Problem:** Running from wrong directory

**Solution:** Make sure you're in the project root directory where `build.sbt` is located

---

## Quick Start (Complete)

```bash
# 1. Copy template
cp cyfra-satellite/copernicus-credentials.template cyfra-satellite/copernicus-credentials.properties

# 2. Edit with your editor
nano cyfra-satellite/copernicus-credentials.properties
# Or: code cyfra-satellite/copernicus-credentials.properties
# Or: notepad cyfra-satellite\copernicus-credentials.properties

# 3. Test
sbt "project satellite" "runMain io.computenode.cyfra.satellite.examples.testRealData"
```

**That's it!** Your credentials are now saved and will be used automatically. 🎉



