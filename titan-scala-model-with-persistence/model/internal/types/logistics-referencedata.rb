# Logistics Reference Data model 
in_namespace('Internal.Logistics.ReferenceData') {
  define('LogisticsBrand') {
    field 'id',                         :integer, :identifier => true
    field 'name',                       :string
    field 'metalCode',                  :integer
  }

  define('LogisticsWarehouse') {
    field 'id',                         :integer, :identifier => true
    field 'name',                       :string
    field 'city',                       :string
  }

  define('LogisticsFinancingBank') {
    field 'id',                         :integer, :identifier => true
    field 'name',                       :string
  }
}
