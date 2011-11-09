in_namespace('Internal.Logistics.Inventory') {

  define('InventoryItem') {
    field 'inventoryId',                :integer, :identifier => true
    field 'audit',                      'Audit'
    field 'parentId',                   :integer_key, :references => 'InventoryItem(inventoryId)'
    field 'groupCompanyId',             :integer        # :references => 'GroupCompany(oid)'
    field 'purchaseNeptuneQuotaId',     :string
    field 'purchaseContract',           :string
    field 'salesContract',              :string
    field 'quotaNumber',                :integer
    field 'shipment',                   :monthYear
    field 'counterParty',               :string
    field 'metalId',                    :integer        # :references => 'Metal(oid)'
    field 'brand',                      :string
    field 'quality',                    'Quality'
    field 'quantity',                   'TradeCapture.Internal.RefinedMetal.Quantity'
    field 'receiptDate',                :datetime, :optional => true
    field 'expectedReceiptDate',        :date, :optional => true
    field 'status',                     :enum, :enumerated => ['Expected', 'Split', 'Received', 'InTransit', 'Confirmed', 'Delivered', 'Cancelled']
    field 'contractualLocationId',      :integer        # :references => 'Location(oid)'
    field 'contractualTermsId',         :integer        # :references => 'ContractualTerms(oid)'
    field 'trafficOperatorId',          :integer        # :references => 'User(oid)'
    field 'comments',                   :string
    field 'allocatedToQuotaId',         :integer        # :references => 'LogisticsQuota(quotaId)'
    field 'allocatedToNeptuneQuotaId',  :string
    field 'deliveredDate',              :date, :optional => true
    field 'warehouseRef',               :string
    field 'bundles',                    :integer, :optional => true
    field 'cargoStatus',                :enum,  :enumerated => ['PHYSICAL', 'ELECTRONICWARRANT']
    field 'ewDate',                     :date, :optional => true
    field 'reserved',                   :boolean
    field 'financeAndCosts',            'LogisticsFinanceAndCosts'
    field 'warehouseId',                :integer, :optional =>true        # : references => 'Warehouse(oid)'
    field 'movementIds',                :list, :element_type => :integer
    field 'receiptChainId',             :string
 }

  define('InventoryWeight') {
    field 'inventoryId',                :integer_key, :references => 'InventoryItem(inventoryId)'
    field 'quantity',                   'TradeCapture.Internal.RefinedMetal.Quantity'
  }

  define('LogisticsEvent', :abstract => true) {
    field 'eventId',                    :integer, :identifier => true
    field 'audit',                      'Audit'
    field 'eventDate',                  :datetime
    field 'comments',                   :string
    field 'confirmed',                  :boolean
    field 'createdbyId',                :integer        # :references => 'User(oid)'
    field 'createdDate',                :datetime
    field 'modifiedDate',               :datetime
  }

  define('InventoryAttribute', :abstract => true) {


    field 'attributeType', :enum,  :enumerated => ['QUANTITY', 'QUALITY', 'BRAND', 'EXPECTEDRECEIPTDATE', 'STATUS', 'TRAFFICOPERATOR',
    'COMMENT', 'QUOTA', 'DELIVEREDDATE', 'TRIGGEREDBY', 'WAREHOUSE', 'CARGOSTATUS', 'EWDATE', 'BUNDLES',
    'RESERVED', 'WAREHOUSEREF', 'RECEIPTDATE', 'FINANCEANDCOSTS', 'GAINLOSS', 'RECEIPTCHAIN']

  }

  define('BrandAttribute',    :extends => 'InventoryAttribute') {
    field 'brand',                      :string
  }

  define('QuantityAttribute', :extends => 'InventoryAttribute') {
    field 'quantity',                   'TradeCapture.Internal.RefinedMetal.Quantity'
  }

  define('GainLossAttribute', :extends => 'InventoryAttribute') {
    field 'quantity',                   'TradeCapture.Internal.RefinedMetal.Quantity'
  }

  define('QualityAttribute',  :extends => 'InventoryAttribute') {
    field 'quality',                    'Quality'
  }

  define('ExpectedReceiptDateAttribute', :extends => 'InventoryAttribute') {
    field 'expectedReceiptDate',       :date, :optional => true
  }

  define('StatusAttribute',   :extends => 'InventoryAttribute') {
    field 'status',             :enum, :enumerated => ['Expected', 'Split', 'Received', 'InTransit', 'Confirmed', 'Delivered', 'Cancelled']
  }

  define('TrafficOperatorAttribute', :extends => 'InventoryAttribute') {
    field 'trafficOperatorId',          :integer, :optional => true    # :references => 'User(oid)'
  }

  define('CommentAttribute',    :extends => 'InventoryAttribute') {
    field 'comment',                    :string
  }

  define('QuotaAttribute',    :extends => 'InventoryAttribute') {
    field 'quotaId',                    :integer # :references => 'LogisticsQuota(quotaId)'
    field 'neptuneQuotaId',             :string
  }

  define('ReceiptDateAttribute',   :extends => 'InventoryAttribute') {
    field 'receiptDate',              :datetime, :optional => true
  }

  define('DeliveredDateAttribute',   :extends => 'InventoryAttribute') {
    field 'deliveredDate',              :date, :optional => true
  }

  define('TriggeredByAttribute',    :extends => 'InventoryAttribute') {
    field 'eventId',                    :integer
  }

  define('LogisticsFinanceAndCostsAttribute', :extends => 'InventoryAttribute') {
    field 'financeAndCosts',                   'LogisticsFinanceAndCosts'
  }

  define('WarehouseAttribute',    :extends => 'InventoryAttribute') {
    field 'warehouseId',                    :integer, :optional => true
  }

  define('CargoStatusAttribute',    :extends => 'InventoryAttribute') {
    field 'cargoStatus',                    :enum,  :enumerated => ['PHYSICAL', 'ELECTRONICWARRANT'], :optional => true
  }

  define('EWDateAttribute',    :extends => 'InventoryAttribute') {
    field 'ewDate',                    :date, :optional => true
  }

  define('BundlesAttribute',    :extends => 'InventoryAttribute') {
    field 'bundles',                    :integer, :optional => true
  }

  define('ReservedAttribute',    :extends => 'InventoryAttribute') {
    field 'reserved',                    :boolean
  }

  define('WarehouseRefAttribute',    :extends => 'InventoryAttribute') {
    field 'warehouseRef',                    :string
  }

  define('ReceiptChainAttribute',    :extends => 'InventoryAttribute') {
    field 'inventoryId',                :integer # :references => 'InventoryItem(inventoryId)'
  }

  define('InventoryEvent', :extends => 'LogisticsEvent') {
    field 'eventType',                  :enum,  :enumerated => ['CREATED', 'WEIGH', 'GENERALEDIT', 'GOODSRECEIPT', 'SPLIT', 'CREATEDFROMSPLIT', 'ALLOCATION', 'FINANCEANDCOSTS', 'DELETEALLOCATION', 'QUOTAAMENDED', 'DELIVERY']
    field 'inventory',                  :integer_key, :references => 'InventoryItem(inventoryId)'
    field 'attributes',                 :list, :element_type => 'InventoryAttribute'
  }

  define('LogisticsSplitResult', :extends => 'LogisticsResponse') {
    field 'createdInventoryIds',                 :list, :element_type => :integer
  }

  define('LogisticsUndoEventResult', :extends => 'LogisticsResponse') {
    field 'createdInventoryIds',                 :list, :element_type => :integer
  }
}

