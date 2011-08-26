# Logistics service
in_namespace('Internal.Logistics.Service') {
  define('LogisticsResponse') {
      field 'code', :enum, :enumerated => ['NONE','LOGISTICS_INVENTORY_ALLOCATED', 'LOGISTICS_INVENTORY_UNALLOCATED', 'DELIVERED_STATUS', 'LOGISTICS_INVENTORY_ASSIGNED_MOVEMENT']
      field 'status',   :enum,  :enumerated => ['OK', 'GENERAL_FAILURE', 'INVALID_PARAMETERS', 'MISSING_PARAMETERS', 'NO_PERMISSION', 'VALIDATION_ERROR', 'NOT_FOUND', 'ALREADY_MODIFIED', 'ALLOCATED', 'REMOVED']
  }
}

