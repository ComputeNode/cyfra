#!/bin/bash
# Run Cyfra Satellite Web Server with GDAL support

set -e

# Set GDAL path (adjust for your system)
export GDAL_PATH="/usr/bin"

# Add GDAL to PATH
export PATH="$PATH:$GDAL_PATH"

echo "Starting Cyfra Satellite Web Server..."
echo "GDAL Path: $GDAL_PATH"
echo ""
echo "Server will be available at: http://localhost:8080"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Run the server
sbt "project satellite" "runMain io.computenode.cyfra.satellite.web.SatelliteWebServer"





