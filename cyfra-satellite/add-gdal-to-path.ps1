# Add GDAL to System PATH
# Run this script as Administrator

$gdalPath = "C:\Program Files (x86)\GDAL"

# Get current PATH
$currentPath = [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::Machine)

# Check if GDAL is already in PATH
if ($currentPath -like "*$gdalPath*") {
    Write-Host "GDAL is already in PATH!" -ForegroundColor Green
} else {
    # Add GDAL to PATH
    $newPath = $currentPath + ";" + $gdalPath
    [Environment]::SetEnvironmentVariable("Path", $newPath, [EnvironmentVariableTarget]::Machine)
    Write-Host "✓ GDAL added to PATH successfully!" -ForegroundColor Green
}

Write-Host ""
Write-Host "To use GDAL in this session, run:" -ForegroundColor Yellow
Write-Host '  $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")' -ForegroundColor Cyan
Write-Host ""
Write-Host "Or open a NEW PowerShell window and verify with:" -ForegroundColor Yellow
Write-Host "  gdal_translate --version" -ForegroundColor Cyan





