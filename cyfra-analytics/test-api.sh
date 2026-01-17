#!/bin/bash

API="http://localhost:8081/api/v1"

echo "=== Customer Segmentation API Test ==="
echo

echo "1. Health Check"
curl -s "$API/../health" | jq .
echo -e "\n"

echo "2. Submit Transactions for Multiple Customers"
for i in {1..300}; do
  CUSTOMER_ID=$((1 + i % 10))
  AMOUNT=$((50 + (i % 100) * 5))
  TIMESTAMP=$(($(date +%s)000 - i * 100000))
  CATEGORY=$((i % 20))
  CHANNEL=$( [ $((i % 3)) -eq 0 ] && echo "mobile_app" || echo "web" )
  DISCOUNT=$( [ $((i % 4)) -eq 0 ] && echo "0.15" || echo "0.0" )
  
  curl -s -X POST "$API/transactions" \
    -H "Content-Type: application/json" \
    -d "{
      \"customerId\": $CUSTOMER_ID,
      \"timestamp\": $TIMESTAMP,
      \"amount\": $AMOUNT,
      \"items\": $((1 + i % 5)),
      \"category\": $CATEGORY,
      \"channel\": \"$CHANNEL\",
      \"discountPct\": $DISCOUNT
    }" > /dev/null
  
  if [ $((i % 50)) -eq 0 ]; then
    echo "  Submitted $i transactions..."
  fi
done
echo "  All transactions submitted!"
echo

echo "3. Wait for processing..."
sleep 3
echo

echo "4. Get Customer 1 Segment"
curl -s "$API/customers/1" | jq .
echo -e "\n"

echo "5. Get Customer 5 Segment"
curl -s "$API/customers/5" | jq .
echo -e "\n"

echo "6. List All Segments"
curl -s "$API/segments" | jq '.segments[] | {name, customerCount, avgLifetimeValue}'
echo -e "\n"

echo "7. Get Segment Summary"
curl -s "$API/segments" | jq '{totalCustomers, lastUpdated, segmentCount: (.segments | length)}'
echo

echo "=== Test Complete ==="
