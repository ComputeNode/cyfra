$API = "http://localhost:8081/api/v1"

Write-Host "=== Customer Segmentation API Test ===" -ForegroundColor Cyan
Write-Host

Write-Host "1. Health Check" -ForegroundColor Yellow
Invoke-RestMethod -Uri "$API/../health" -Method Get | ConvertTo-Json
Write-Host

Write-Host "2. Submit Transactions for Multiple Customers" -ForegroundColor Yellow
for ($i = 1; $i -le 300; $i++) {
    $customerId = 1 + ($i % 10)
    $amount = 50 + (($i % 100) * 5)
    $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds() - ($i * 100000)
    $category = $i % 20
    $channel = if ($i % 3 -eq 0) { "mobile_app" } else { "web" }
    $discount = if ($i % 4 -eq 0) { 0.15 } else { 0.0 }
    
    $body = @{
        customerId = $customerId
        timestamp = $timestamp
        amount = $amount
        items = 1 + ($i % 5)
        category = $category
        channel = $channel
        discountPct = $discount
    } | ConvertTo-Json
    
    Invoke-RestMethod -Uri "$API/transactions" -Method Post -Body $body -ContentType "application/json" | Out-Null
    
    if ($i % 50 -eq 0) {
        Write-Host "  Submitted $i transactions..."
    }
}
Write-Host "  All transactions submitted!" -ForegroundColor Green
Write-Host

Write-Host "3. Wait for processing..." -ForegroundColor Yellow
Start-Sleep -Seconds 3
Write-Host

Write-Host "4. Get Customer 1 Segment" -ForegroundColor Yellow
Invoke-RestMethod -Uri "$API/customers/1" -Method Get | ConvertTo-Json -Depth 5
Write-Host

Write-Host "5. Get Customer 5 Segment" -ForegroundColor Yellow
Invoke-RestMethod -Uri "$API/customers/5" -Method Get | ConvertTo-Json -Depth 5
Write-Host

Write-Host "6. List All Segments" -ForegroundColor Yellow
$segments = Invoke-RestMethod -Uri "$API/segments" -Method Get
$segments.segments | ForEach-Object {
    [PSCustomObject]@{
        Name = $_.name
        CustomerCount = $_.customerCount
        AvgLifetimeValue = [math]::Round($_.avgLifetimeValue, 2)
    }
} | Format-Table
Write-Host

Write-Host "7. Get Segment Summary" -ForegroundColor Yellow
[PSCustomObject]@{
    TotalCustomers = $segments.totalCustomers
    LastUpdated = $segments.lastUpdated
    SegmentCount = $segments.segments.Count
} | ConvertTo-Json
Write-Host

Write-Host "=== Test Complete ===" -ForegroundColor Cyan
