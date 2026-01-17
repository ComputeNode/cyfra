# Customer Segmentation Microservice

GPU-accelerated customer segmentation using continuous Fuzzy C-Means clustering with fs2 streaming.

## Architecture

### Clean Separation of Concerns

```
├── model/              Domain models (Transaction, CustomerProfile, CustomerSegment)
├── repository/         Data persistence layer
│   ├── CustomerProfileRepository      Customer behavioral profiles
│   ├── CustomerSegmentRepository      Segment assignments
│   └── CentroidsRepository            Segment centroids (configurable!)
├── service/            Business logic
│   ├── FeatureExtractionService      Compute RFM + behavioral features
│   ├── DataGenerationService         Generate sample data
│   └── SegmentationService           Orchestrates GPU clustering pipeline
├── endpoints/          Tapir API definitions
└── server/             HTTP server setup
```

### fs2 Streaming Pipeline

```
POST /transactions → Queue → fs2 Stream
                                ↓
                          Cache & Extract Features
                                ↓
                          Batch (256 customers)
                                ↓
                      GPU Fuzzy C-Means Clustering
                                ↓
                      Assign Segments → Repository
                                ↓
                            (loop continuously)
```

## Features

### 🎯 **Dynamic Centroids**
Centroids are **not hardcoded** - they're stored in the repository and can be updated via API:

```bash
# Get current centroids
curl http://localhost:8081/api/v1/centroids

# Update centroids
curl -X PUT http://localhost:8081/api/v1/centroids \
  -H "Content-Type: application/json" \
  -d '{
    "labels": ["VIP", "Loyal", "Price Sensitive", "New", "At Risk"],
    "values": [0.1, 0.9, ..., 0.2]
  }'
```

### 📊 **Sample Data Generation**
On startup, the service generates **100 sample customers** with **20 transactions each**, pre-distributed across segments.

### 🗂️ **Separate Repositories**
- **CustomerProfileRepository**: Stores customer behavioral profiles (features, spend, etc.)
- **CustomerSegmentRepository**: Stores segment assignments with fuzzy memberships
- **CentroidsRepository**: Stores segment definitions (centroids)
- **No transaction storage**: Transactions are processed in-flight and cached temporarily

### 🧩 **Modular Services**
- **FeatureExtractionService**: Standalone feature computation (10 RFM + behavioral metrics)
- **DataGenerationService**: Generates realistic sample data
- **SegmentationService**: Lightweight orchestrator (~165 lines)

## API Endpoints

### Submit Transaction
```bash
POST /api/v1/transactions
{
  "customerId": 123,
  "timestamp": 1705147800000,
  "amount": 149.99,
  "items": 3,
  "category": 5,
  "channel": "mobile_app",
  "discountPct": 0.15
}
```

### Get Customer Segment
```bash
GET /api/v1/customers/123
```

Response:
```json
{
  "customerId": 123,
  "segment": "VIP Champion",
  "confidence": 0.78,
  "topSegments": [
    ["VIP Champion", 0.78],
    ["Loyal Customer", 0.15],
    ["Price Sensitive", 0.07]
  ],
  "lifetimeValue": 4567.89,
  "daysSinceLastTransaction": 2,
  "transactionCount": 45
}
```

### List All Segments
```bash
GET /api/v1/segments
```

### Get/Update Centroids
```bash
GET /api/v1/centroids
PUT /api/v1/centroids
```

### Health Check
```bash
GET /health
```

## Running

```bash
# Start server (auto-generates sample data)
sbt "analytics/run"

# Server runs on http://localhost:8081
# Swagger UI at http://localhost:8081/docs

# Test API
./cyfra-analytics/test-api.ps1   # Windows
./cyfra-analytics/test-api.sh    # Linux/Mac
```

## Customer Segments

The service classifies customers into 5 behavioral segments (configurable via API):

1. **VIP Champion** - High recency, frequency, and monetary value
2. **Loyal Customer** - Consistent purchasers with good retention
3. **Price Sensitive** - Discount-driven, shops on deals
4. **New Explorer** - Recent acquisition, exploring catalog
5. **At Risk** - Low recency, needs re-engagement

## Technical Details

- **Features**: 10-dimensional (RFM + order metrics + behavioral patterns)
- **Clustering**: Fuzzy C-Means with fuzziness=2.0, 10 iterations per batch
- **Batch Size**: 256 customers processed together on GPU
- **Sample Data**: 100 customers, 20 transactions each, generated on startup
- **Pipeline**: Continuous fs2 stream with GPU acceleration via `GCluster`

## Architecture Improvements

### ✅ Separated Repositories
- `CustomerProfileRepository` - Behavioral profiles
- `CustomerSegmentRepository` - Segment assignments
- `CentroidsRepository` - Configurable centroids

### ✅ Extracted Services
- `FeatureExtractionService` - Standalone feature computation
- `DataGenerationService` - Sample data generation
- `SegmentationService` - Lightweight orchestrator

### ✅ No Transaction Storage
Transactions are processed in streaming fashion and cached temporarily (last 50 per customer) for feature extraction. No persistent transaction storage needed.

### ✅ Configurable Centroids
Centroids stored in repository with PUT endpoint to update them dynamically. No hardcoding!

## Swagger UI

API documentation available at: `http://localhost:8081/docs`

## Tests

```bash
# Run all tests
sbt "analytics/test"
```

Tests cover:
- Sample data generation
- Continuous pipeline processing
- API endpoints (health, segments, centroids)
