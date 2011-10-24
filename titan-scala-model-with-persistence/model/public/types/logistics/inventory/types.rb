in_namespace('EDM') {
  in_namespace('Logistics') {
  	in_namespace('Inventory') {

     define('Assignment') {
       constant 'PURCHASE', 'P'
       constant 'SALE', 'S'
       field 'oid',                        :integer_id
       field 'quotaName',                  :string
       field 'inventoryId',                :integer
       field 'direction',                  :string
     }

     define('CargoStatus', :abstract => true) {
     }

     define('PhysicalInventory', :extends => 'CargoStatus') {
     }

     define('ElectronicWarrant', :extends => 'CargoStatus') {
       field 'warrantDate',                :datetime
     }

     define('InventoryItem') {
       field 'oid',                        :integer_id
       field 'tradeId',                    :integer
       field 'parentId',                   :integer, :optional => true
       field 'purchaseAssignment',         'EDMAssignment'
       field 'salesAssignment',            'EDMAssignment', :optional => true
       field 'quantity',                   :EQuantity
       field 'status',                     :enum, :enumerated => ['Expected', 'Split', 'Received', 'InTransit', 'ProvisionalDelivery', 'FinalDelivery', 'Cancelled']
       field 'trafficOperatorId',          :integer
       field 'productSpec',                :ProductSpec   
       field 'expectedReceiptDate',        :date
       field 'receiptDate',                :date, :optional => true
       field 'deliveredDate',              :date, :optional => true
       field 'deleted',                    :boolean
       field 'cargoStatus',                'CargoStatus'
       field 'warehouse',                  :string, :optional => true
     }
                                 
     define('AssignmentItem', :extends => 'Assignment') {
       field 'quantity',	           :EQuantity
       field 'productSpec',                :ProductSpec
     }
    }

  }
}
