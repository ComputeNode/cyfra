# Run Cyfra Satellite Web Server with GDAL support
# This script sets up the environment and starts the server

$ErrorActionPreference = "Stop"

# Set GDAL path
$env:GDAL_PATH = "C:\Program Files (x86)\GDAL"

# Add GDAL to PATH for this session
$env:Path = $env:Path + ";$env:GDAL_PATH"

Write-Host "Starting Cyfra Satellite Web Server..." -ForegroundColor Green
Write-Host "GDAL Path: $env:GDAL_PATH" -ForegroundColor Cyan
Write-Host ""
Write-Host "Server will be available at: http://localhost:8080" -ForegroundColor Yellow
Write-Host ""
Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Gray
Write-Host ""

# Run the server
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"





