# Logistics Reference Data service
in_namespace('Internal.Logistics.ReferenceData') {
  service('LogisticsReferenceDataService') {
    operation('GetBrands', :returns => list('LogisticsBrand')) {
    }
    operation('GetWarehouses', :returns => list('LogisticsWarehouse')) {
    }
    operation('GetFinancingBanks', :returns => list('LogisticsFinancingBank')) {
    }
    operation('CreateWarehouse', :returns => :LogisticsWarehouse) {
      parameter 'name',             :string
    }
    operation('UpdateWarehouse', :returns => :LogisticsWarehouse) {
      parameter 'oid',             :integer
      parameter 'name',             :string
    }
    operation('CreateBrand', :returns => :LogisticsBrand) {
      parameter 'commodity',        :integer
      parameter 'name',             :string
    }
    operation('UpdateBrand', :returns => :LogisticsBrand) {
      parameter 'oid',              :integer
      parameter 'newName',          :string
    }
  }
}
