#// This is the EDM logistics inventory service interface definition

in_namespace('EDM.Logistics.Inventory') {
  service('EdmInventoryService') {
    operation('GetInventoryById', :returns => :EDMInventoryItem) {
      parameter 'inventoryId', :integer
    }
    operation('GetInventoryTreeByPurchaseQuotaId', :returns => list('EDMInventoryItem')) {
      parameter 'quotaId', :string
    }
  }
  service('EdmAssignmentService') {
    operation('GetAllSalesAssignments', :returns => list('EDMAssignmentItem')) {
    }
    operation('GetAssignmentById', :returns => :EDMAssignmentItem) {
      parameter 'assignmentId', :integer  
    }
  }

}
