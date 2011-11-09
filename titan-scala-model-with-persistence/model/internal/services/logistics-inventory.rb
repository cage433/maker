in_namespace('Internal.Logistics.Inventory') {
  service('LogisticsInventoryService') {
    operation('GetAll', :returns => list('InventoryItem')) {
    }

    operation('GeneralEdit', :returns => :LogisticsResponse) {
      parameter 'inventoryId',       :integer
      parameter 'eventDate',         :datetime
      parameter 'confirmed',         :boolean
      parameter 'quality',           :Quality
      parameter 'brand',             :string
      parameter 'warehouseId',       :integer
      parameter 'status',            :string
      parameter 'deliveredDate',     :date
      parameter 'expectedReceiptDate', :date
      parameter 'cargoStatus',       :string
      parameter 'ewDate',            :date 		# :optional => true
      parameter 'bundles',           :integer
      parameter 'reserved',          :boolean
      parameter 'trafficOperatorId', :integer
      parameter 'warehouseRef',      :string
      parameter 'comments',          :string
    }

    operation('AdjustWeights', :returns => :LogisticsSplitResult) {
      parameter 'targetInventoryId', :integer
      parameter 'eventDate',        :datetime
      parameter 'confirmed',        :boolean
      parameter 'inventoryWeights', list('InventoryWeight')
      parameter 'splitQuantity',     :'TradeMgmt.Internal.RefinedMetal.Quantity'
      parameter 'gainLossQuantity',  :'TradeMgmt.Internal.RefinedMetal.Quantity'
    }

    operation('Weigh', :returns => :LogisticsResponse) {
      parameter 'inventoryId', :integer
      parameter 'eventDate',   :datetime
      parameter 'confirmed',   :boolean
      parameter 'quantity',    :'TradeMgmt.Internal.RefinedMetal.Quantity'
    }

    operation('GoodsReceipted', :returns => :LogisticsSplitResult) {
      parameter 'inventoryId',             :integer
      parameter 'eventDate',               :datetime
      parameter 'confirmed',               :boolean
      parameter 'quality',                 :Quality
      parameter 'brand',                   :string
      parameter 'quantity',                :'TradeMgmt.Internal.RefinedMetal.Quantity'
      parameter 'trafficOperatorId',       :integer
      parameter 'comments',                :string
      parameter 'warehouseId',             :integer
      parameter 'warehouseRef',            :string
      parameter 'cargoStatus',             :string
      parameter 'receiptDate',             :date
      parameter 'transferredAdjustments',  :list, :element_type => :InventoryWeight, :optional => true
      parameter 'splitAdjustmentQuantity', :'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
    }

    operation('Split', :returns => :LogisticsSplitResult) {
      parameter 'inventoryId', :integer
      parameter 'eventDate',   :datetime
      parameter 'confirmed',   :boolean
      parameter 'quantities',  list('TradeMgmt.Internal.RefinedMetal.Quantity')
    }

    operation('Delivery', :returns => :LogisticsSplitResult) {
      parameter 'targetInventoryId', :integer
      parameter 'eventDate',         :datetime
      parameter 'confirmed',         :boolean
      parameter 'deliveredDate',     :date
      parameter 'inventoryWeights',  list('InventoryWeight')
      parameter 'splitQuantity',     :'TradeMgmt.Internal.RefinedMetal.Quantity'
      parameter 'gainLossQuantity',  :'TradeMgmt.Internal.RefinedMetal.Quantity'
    }

    operation('GetEventsByEventType', :returns => list('InventoryEvent')) {
      parameter 'inventoryId', :integer
      parameter 'eventType',   :string
    }

    operation('GetEventsMatchingAttributeTypes', :returns => list('InventoryEvent')) {
      parameter 'inventoryId',    :integer
      parameter 'attributeTypes', list(:string)
    }

    operation('GetAllEvents', :returns => list('InventoryEvent')) {
      parameter 'inventoryId',    :integer
    }

    operation('GetInventory', :returns => :InventoryItem) {
      parameter 'inventoryId',    :integer
    }

    operation('GetInventoryList', :returns => list('InventoryItem')) {
      parameter 'inventoryIds',    list(:integer)
    }

    operation('UndoEvent', :returns => :LogisticsUndoEventResult) {
      parameter 'eventId',    :integer
    }
  }
}
