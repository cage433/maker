service('LogisticsInventoryService') {

  operation('GetAll', :required_permissions => ['permission.logistics.read']) { }

  operation('GetEventsByEventType', :required_permissions => ['permission.logistics.read']) { }

  operation('GetEventsMatchingAttributeTypes', :required_permissions => ['permission.logistics.read']) { }

  operation('GetAllEvents', :required_permissions => ['permission.logistics.read']) { }

  operation('GetInventory', :required_permissions => ['permission.logistics.read']) { }

  operation('GetInventoryList', :required_permissions => ['permission.logistics.read']) { }


  operation('GeneralEdit', :required_permissions => ['permission.logistics.write']) { }

  operation('AdjustWeights', :required_permissions => ['permission.logistics.write']) { }

  operation('Weigh', :required_permissions => ['permission.logistics.write']) { }

  operation('GoodsReceipted', :required_permissions => ['permission.logistics.write']) { }

  operation('Split', :required_permissions => ['permission.logistics.write']) { }

  operation('UndoEvent', :required_permissions => ['permission.logistics.write']) { }

}

service('LogisticsFinanceAndCostsService') {

  operation('FinanceAndCosts', :required_permissions => ['permission.logistics.write']) { }

}

service('LogisticsQuotaService') {

  operation('GetQuotas', :required_permissions => ['permission.logistics.read']) { }

  operation('GetQuota', :required_permissions => ['permission.logistics.read']) { }


  operation('SetComments', :required_permissions => ['permission.logistics.write']) { }

}

service('LogisticsReferenceDataService') {

  operation('GetBrands', :required_permissions => ['permission.logistics.read']) { }

  operation('GetWarehouses', :required_permissions => ['permission.logistics.read']) { }

  operation('GetFinancingBanks', :required_permissions => ['permission.logistics.read']) { }


  operation('CreateBrand', :required_permissions => ['permission.administrator']) { }

  operation('UpdateBrand', :required_permissions => ['permission.administrator']) { }

  operation('CreateWarehouse', :required_permissions => ['permission.administrator']) { }

  operation('UpdateWarehouse', :required_permissions => ['permission.administrator']) { }

}

service('LogisticsAllocationService') {

  operation('Allocate', :required_permissions => ['permission.logistics.write']) { }

  operation('DeleteAllocation', :required_permissions => ['permission.logistics.write']) { }

}

service('LogisticsMovementService') {

  operation('GetAll', :required_permissions => ['permission.logistics.read']) { }

  operation('GetSummary', :required_permissions => ['permission.logistics.read']) { }

  operation('GetDetail', :required_permissions => ['permission.logistics.read']) { }

  
  operation('Add', :required_permissions => ['permission.logistics.write']) { }

  operation('Update', :required_permissions => ['permission.logistics.write']) { }
  
}
