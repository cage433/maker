in_namespace('Internal.Logistics.Allocation') {
  define('LogisticsAllocateResult', :extends => 'LogisticsResponse') {
    field 'allocatedInventoryId',                 :integer_key, :references => 'InventoryItem(inventoryId)'
  }
}
