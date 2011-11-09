in_namespace('Internal.Logistics.Allocation') {
  service('LogisticsAllocationService') {
    operation('Allocate', :returns => :LogisticsAllocateResult) {
      parameter 'inventoryId', :integer
      parameter 'quantity',           'TradeMgmt.Internal.RefinedMetal.Quantity'
      parameter 'quotaId',   :integer
      parameter 'fullyAllocated',   :boolean
      parameter 'confirmed',   :boolean
    }

    operation('DeleteAllocation', :returns => :LogisticsResponse) {
      parameter 'inventoryId', :integer
      parameter 'confirmed',   :boolean
    }
  }
}
