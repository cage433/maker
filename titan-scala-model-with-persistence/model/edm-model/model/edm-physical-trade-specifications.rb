in_namespace('EDM.PhysicalTradeSpecs') {

  define('DeliveryLocation') {
    field 'oid',                            :integer, :identifier => true
    field 'location',                       :guid
    field 'incoterm',                       :string
    field 'dutyPaid',                       :boolean
  }

  define('DeliverySpec') {
    field 'oid',                            :integer, :identifier => true
    field 'quantity',                        :'EDM.shared.types.Quantity'
    field 'tolerance',                       :'Tolerance'
    field 'schedule',                        :'DateSpec'
    field 'deliveryLocations',               :list, :element_type => 'DeliveryLocation'
    field 'destinationLocation',             :guid # this is the initial 'best guess' as to where the materials will go
    field 'materialSpec',                    :'MaterialSpec'
  }

  define('VATSpec') {
    field 'invoiceDate',           :date, :optional => true
    field 'invoiceType',           :enum, :enumerated => ['Issued', 'Received'], :optional => true
  }

  define('QuotaDetail') {
    field 'oid',                            :integer, :identifier => true
    field 'id',                             :string
    field 'comments',                       :string, :max_length => Transaction::COMMENTLENGTH
    field 'quantityOptionality',            :enum, :enumerated => ['Buyer', 'Seller', 'None'], :optional => true
    field 'tradeOptionality',               :enum, :enumerated => ['Buyer', 'Seller', 'None'], :optional => true
#    field 'paymentSpec',                    'EDMPaymentSpec'
    field 'securityTerms',                  :guid
    field 'deliverySpecs',                  :list, :element_type =>'DeliverySpec' #1..*
    field 'pricingSpec',                    'EDMPricingSpec'
    field 'vatSpec',                        'VATSpec'
    field 'benchmark',                      :string
    field 'expectedSales',                  :'DateSpec'
  }

  define('EDMQuota') {
    field 'oid',                            :integer, :identifier => true
    field 'quotaNumber',                    :integer
    field 'detail',                         :'QuotaDetail'
  }

  define('PhysicalTradeExecution', :extends => 'Execution') {
    field 'quota',                          :'EDM.PhysicalTradeSpecs.EDMQuota'
  }
  
  define('PhysicalTradeSpec', :extends => 'TradeSpec') {
    field 'fullyOperated',                  :boolean
#    field 'quotaDetails',                   :list, :element_type => 'QuotaDetail' #1..*
#    field 'materialSpecs',                  :list, :element_type =>'MaterialSpec' #0..*
  }
}

in_namespace('EDM.Trades') {
    define('PhysicalTrade', :extends => 'EDM.Trades.OTCTrade') {
      constant 'NEPTUNE_PREFIX', 'NEPTUNE:'
      field 'id',                               :string
      field 'quotas',                           :list, :element_type => 'EDM.PhysicalTradeSpecs.EDMQuota' #1..*
    }

    # used internally by the trade service to record metadata
    #// about the booking
    define('QuotaMetadata') {
      field 'quotaNumber',                       :integer
      field 'neptuneFxPricing',                  :boolean
    }
    define('TradeMetadata') {
#      field 'oid',                               :integer, :identifier => true
      field 'tradeOid',                          :integer
      field 'source',                            :enum, :enumerated => ['Neptune', 'Titan'] 
      field 'quotas',                            :list, :element_type => 'EDM.Trades.QuotaMetadata'
    }
}
