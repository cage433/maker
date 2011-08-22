in_namespace('Internal.Logistics.Inventory.FinanceAndCosts') {

  service('LogisticsFinanceAndCostsService') {

    operation('FinanceAndCosts', :returns => :LogisticsResponse) {
    	parameter 'inventoryId',         :integer
    	parameter 'eventDate',           :datetime
    	parameter 'pledged',             :boolean
    	parameter 'financingBank',       :integer
    	parameter 'financeDate',         :date  
    	parameter 'financeReleasedDate', :date 
    	parameter 'pledgeBillNo',        :string
    	parameter 'pledgeFee',           :real
    	parameter 'releaseFee',          :real
    	parameter 'storageFee',          :real
    	parameter 'rentalFee',           :real  
    	parameter 'rentalDate',          :date 
    	parameter 'ewFee',               :real
    	parameter 'warehouseFee',        :real
    }

  }
}
