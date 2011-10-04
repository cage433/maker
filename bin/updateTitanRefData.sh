#!/bin/bash
pushd .
cd ..
sbt/sbt ";project services ;run-main starling.services.rpc.refdata.RefDataServices"

cp /tmp/allEdmTrades.json.zip services/resources/tests/valuationservice/testdata/allEdmTrades.json.zip 
cp /tmp/exchanges.json services/resources/tests/valuationservice/testdata/exchanges.json 
cp /tmp/metals.json services/resources/tests/valuationservice/testdata/metals.json 
cp /tmp/uoms.json services/resources/tests/valuationservice/testdata/uoms.json

sbt/sbt ";project services ;run-main starling.services.rpc.logistics.LogisticServices"
cp /tmp/logisticsEdmAllAssignments.json services/resources/tests/valuationservice/testdata/logisticsEdmAllAssignments.json 
cp /tmp/logisticsEdmAllSalesAssignments.json services/resources/tests/valuationservice/testdata/logisticsEdmAllSalesAssignments.json 
cp /tmp/logisticsEdmInventory.json services/resources/tests/valuationservice/testdata/logisticsEdmInventory.json

popd
echo Done
