#version '0.2'
default_version '1'


# This is the point of entry into the DSL. Classes are imported here, in turn these may import other classes.

# audit
import 'audit.rb'

# events
import 'event.rb'

# types
import 'refined-metal-trade-types.rb'

# model
import 'permission-model.rb'
import 'refined-metal-reference-data-model.rb'
import 'refined-metal-reference-data-translations-model.rb'
import 'edm-model/model/edm-types.rb'
import 'edm-model/model/edm-pricing-schedules.rb'
import 'edm-model/model/edm-transactions.rb'
import 'edm-model/model/edm-trades.rb'
import 'edm-model/model/edm-trade-pricing-types.rb'
import 'refined-metal-trade-pricing-types.rb'
import 'edm-model/model/edm-material-specification.rb'
import 'edm-model/model/edm-payment-specification.rb'
import 'edm-model/model/edm-physical-trade-specifications.rb'
import 'refined-metal-trade-model.rb'
import 'refined-metal-trade-document-model.rb'

# services
import 'permission-service.rb'
import 'trade-document-service.rb'
import 'refined-metal-trade-service.rb'
import 'refined-metal-document-service.rb'
import 'refined-metal-reference-data-service.rb'
import 'costsandincomes-service.rb'
import 'mtmpnl-service.rb'
import 'edm-model/services/edm-trade-service.rb'

# Reference Data model and services
import 'reference-data/master-model-private.rb'
import 'reference-data/public/master-model.rb'

# test service and model
import 'test-model.rb'
import 'test-service.rb'

#
# Public
#

## Types
import 'public/types/logistics/inventory/types.rb'

## Services
import 'public/services/logistics/inventory/services.rb'

#
# Internal
#

## Types
import 'internal/types/logistics-quota.rb'
import 'internal/types/logistics-service.rb'
import 'internal/types/logistics-inventory-financeandcosts.rb'
import 'internal/types/logistics-inventory.rb'
import 'internal/types/logistics-referencedata.rb'
import 'internal/types/logistics-allocation.rb'
import 'internal/types/logistics-movement.rb'
import 'internal/types/logistics-task.rb'

## Services
import 'internal/services/logistics-allocation.rb'
import 'internal/services/logistics-inventory.rb'
import 'internal/services/logistics-inventory-financeandcosts.rb'
import 'internal/services/logistics-quota.rb'
import 'internal/services/logistics-referencedata.rb'
import 'internal/services/logistics-movement.rb'
import 'internal/services/logistics-task.rb'
import 'internal/services/stub-tradeservice.rb'

