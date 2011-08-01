in_namespace('Internal.Logistics.Movement') {
  service('LogisticsMovementService') {
    operation('GetAll', :returns => list('MovementSummary')) {
    }

    operation('GetSummary', :returns => :MovementSummary) {
      parameter 'movementId',    :integer
    }

    operation('GetSummaries', :returns => list('MovementSummary')) {
      parameter 'movementIds',   list(:integer)
    }

    operation('GetDetail', :returns => :MovementDetail) {
      parameter 'movementId',    :integer
    }

    operation('Add', :returns => :MovementResponse) {
    }

    operation('Update', :returns => :MovementResponse) {
      parameter 'detail',    'MovementDetail'	
    }
    
    operation('GetInventory', :returns => list(:integer)) {
      parameter 'movementId', :integer
    }
    
    operation('AddInventory', :returns => :MovementResponse) {
      parameter 'movementId', :integer
      parameter 'inventoryId', :integer
    }
    
    operation('RemoveInventory', :returns => :MovementResponse) {
      parameter 'movementId', :integer
      parameter 'inventoryIds', list(:integer)
    }    
  }
}
