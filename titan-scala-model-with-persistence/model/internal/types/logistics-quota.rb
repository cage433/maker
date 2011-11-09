# Logistics model 
in_namespace('Internal.Logistics.Quota') {

  define('LogisticsQuota') {
    field 'quotaId',                    :integer, :identifier => true
    field 'audit',                      'Audit'
    field 'tradeCaptureId',             :integer
    field 'neptuneQuotaId',             :string
    field 'purchase',                   :boolean
    field 'contract',                   :string
    field 'quotaNumber',                :integer
    field 'shipment',                   :monthYear
    field 'metalId',                    :integer        # :references => 'Metal(oid)'
    field 'quality',                    'Quality'
    field 'quantity',                   'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'groupCompanyId',             :integer        # :references => 'GroupCompany(oid)'
    field 'counterParty',               :string
    field 'contractualLocationId',      :integer        # :references => 'Location(oid)'
    field 'contractualTermsId',         :integer        # :references => 'ContractualTerms(oid)'
    field 'trafficOperatorId',          :integer        # :references => 'User(oid)'
    field 'fullyAllocated',             :boolean
    field 'allocatedQuantity',          'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'unallocatedQuantity',        'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'expectedSalesMonth',         :monthYear
    field 'expectedSalesLocationId',    :integer        # :references => 'Location(oid)'
    field 'comments',                   :string
    field 'active',                     :boolean
  }
}

