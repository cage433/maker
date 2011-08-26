in_namespace('Internal.Logistics.Inventory.FinanceAndCosts') {

  define('LogisticsFinanceAndCosts') {
    field 'inventoryId',         :integer_key, :references => 'InventoryItem(inventoryId)'
    field 'pledged',             :boolean
    field 'financingBank',       :integer,:optional => true  # :references => 'LogisticsFinancingBank(oid)'
    field 'financeDate',         :date,   :optional => true
    field 'financeReleasedDate', :date,   :optional => true
    field 'pledgeBillNo',        :string, :optional => true
    field 'pledgeFee',           :real,   :optional => true
    field 'releaseFee',          :real,   :optional => true
    field 'storageFee',          :real,   :optional => true
    field 'rentalFee',           :real,   :optional => true
    field 'rentalDate',          :date,   :optional => true
    field 'ewFee',               :real,   :optional => true
    field 'warehouseFee',        :real,   :optional => true
  }
}
