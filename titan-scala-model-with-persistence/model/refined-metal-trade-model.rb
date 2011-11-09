expose 'RefinedMetalTrade'
expose 'TradeBlotterRow'
expose 'TradeMgmt.Internal.RefinedMetal.Quota'
expose 'TradeBlotterQuotaRow'
expose 'TradeWarning'

# this is the model as used for Release 1 of RMET
in_namespace('TradeMgmt.Internal.RefinedMetal') {

  define('PaymentMethod') {
    field 'oid',                            :integer, :identifier => true
  }
  
  define('TelegraphicTransfer', :extends => 'PaymentMethod') {
    field 'daysGrace',                      :integer
  }

  define('LetterOfCredit', :extends => 'PaymentMethod') {
    field 'tolerance',                      :'Tolerance'
  }
  
  define('OpenAccount', :extends => 'PaymentMethod'){
  }
  
  define('Guarantee') {
    field 'amount',                         :'EDM.Common.Units.Quantity'
    field 'requiredBy',                     :date
  }
     
  define('PaymentDate') {
    field 'oid',                            :integer, :identifier => true
#     field 'referenceEvent',               :'TradeEvent'
     field 'daysOffset',                    :integer
  }

  define('PresentedDocument') {
    field 'oid',                            :integer, :identifier => true
    field 'documentType',                   :integer_key, :references => 'TradeMgmt.Internal.RefinedMetal.DocumentType(oid)'
    field 'copy',                           :boolean
  }
  
  define('PaymentSpec') {
    constant 'notesLength', 4000
    field 'paymentCurrency',                   :integer_key, :optional => true, :references => 'TradeMgmt.Internal.RefinedMetal.Currency(oid)'
    field 'sellerChargesRechargedToBuyer',     :boolean, :optional => true
    field 'riskApproved',                      :boolean, :optional => true
    field 'notes',                             :string, :optional => true, :max_length => PaymentSpec::NOTESLENGTH
    field 'paymentMethods',                    :list, :element_type => 'PaymentMethod'
    # artificially shortened name to avoid tooling bugs
    field 'gt',                                :Guarantee, :optional => true
    field 'paymentDates',                      :list, :element_type => 'PaymentDate'
    field 'presentedDocuments',                :list, :element_type => 'PresentedDocument'
  }

  define('TradeWarning') {
    field 'oid',                      :integer, :identifier => true
    field 'tradeId',                  :integer_key, :references => 'RefinedMetalTrade(oid)'
    field 'warningType',              :enum, :enumerated => ['CompletionFailure']
    field 'externalId',               :string
    field 'deleted',                  :boolean
  }

  define('Quota') {
    constant 'materialGradeCommentLength', 255
    constant 'premiumCommentLength',  255
    field 'oid',                      :integer, :identifier => true
    field 'quotaNumber',              :integer
    field 'quotaName',                :string
    field 'quantity',                 'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'quality',                  'Quality'
    field 'contractualLocation',      :integer_key, :optional => true, :references => 'Location(oid)'
    field 'contractualTerms',         :integer_key, :optional => true, :references => 'ContractualTerms(oid)'
    field 'securityTerms',            :integer_key, :optional => true, :references => 'SecurityTerms(oid)'
    field 'paymentTerms',             :integer_key, :optional => true, :references => 'PaymentTerms(oid)'
    field 'qpType',                   :integer_key, :optional => true, :references => 'TradeMgmt.Internal.RefinedMetal.QPType(oid)'
    field 'market',                   :integer_key, :optional => true, :references => 'Market(oid)'
    field 'currency',                 :integer_key, :optional => true, :references => 'TradeMgmt.Internal.RefinedMetal.Currency(oid)'
    field 'shipment',                 :monthYear
    field 'expectedSales',            :monthYear
    field 'premium',                  'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'premiumCcy',               :integer_key, :optional => true, :references => 'TradeMgmt.Internal.RefinedMetal.Currency(oid)'
    field 'materialGradeComments',    :string
    field 'premiumComments',          :string
    field 'destinationLocation',      :integer_key, :optional => true, :references => 'Location(oid)'
    field 'destinationTerms',         :integer_key, :optional => true, :references => 'ContractualTerms(oid)'
    field 'fxPricing',                :boolean
    field 'pricingOffset',            :integer_key, :optional => true, :references => 'PricingOffset(oid)'
    field 'pricingSpecs',             :list, :element_type => 'PricingSpec'
    field 'vatInvoiceDate',           :date, :optional => true
    field 'vatInvoiceType',           :enum, :enumerated => ['Issued', 'Received'], :optional => true
    field 'deferredCreditDays',       :integer, :optional => true
    field 'paySp',                    'TradeMgmt.Internal.RefinedMetal.PaymentSpec'
  }
  
  # as displayed in the trade blotter grid
  define('TradeBlotterQuotaRow') {
    field 'tradeOID',                  :integer
    field 'quotaOID',                  :integer
    field 'quotaNumber',               :integer
    field 'quantity',                  :real,    :optional => true
    field 'uom',                       :string
    field 'uomId',                     :integer, :optional => true
    field 'grade',                     :string
    field 'gradeId',                   :integer, :optional => true
    field 'shape',                     :string
    field 'shapeId',                   :integer, :optional => true
    field 'contractualLocation',       :string
    field 'contractualLocationId',     :integer, :optional => true
    field 'contractualTerms',          :string
    field 'contractualTermsId',        :integer, :optional => true
    field 'salesTradingRegion',        :string
    field 'salesTradingRegionId',      :integer, :optional => true
    field 'securityTerms',             :string
    field 'securityTermsId',           :integer, :optional => true
    field 'paymentTerms',              :string
    field 'paymentTermsId',            :integer, :optional => true
    field 'qpTypeId',                  :integer, :optional => true
    field 'qpType',                    :string
    field 'market',                    :string
    field 'marketId',                  :integer, :optional => true
    field 'currency',                  :string
    field 'currencyId',                :integer, :optional => true
    field 'shipment',                  :monthYear
    field 'expectedSales',             :monthYear
    field 'premium',                   :real,    :optional => true
    field 'premiumUom',                :string
    field 'premiumUomId',              :integer, :optional => true
    field 'premiumCcy',                :string
    field 'premiumCcyId',              :integer, :optional => true
    field 'premiumComments',           :string
    field 'gradeComments',             :string
    field 'pricingOffsetId',           :integer, :optional => true
    field 'pricingOffset',             :string
    field 'destinationLocationId',     :integer, :optional => true
    field 'destinationLocation',       :string
    field 'destinationTermsId',        :integer, :optional => true
    field 'destinationTerms',          :string
    field 'presentedDocumentId',       :integer, :optional => true
    field 'presentedDocumentName',     :string
    field 'presentedDocumentIsCopy',   :boolean
    field 'numberOfDeferredCreditDays',:integer
    field 'vatInvoiceDate',            :date, :optional => true
    field 'vatInvoiceType',            :enum, :enumerated => ['Issued', 'Received'], :optional => true
  }

  # an object to use to represent a relationship which can optionally point to the counterparty table or the nonkyccounterparty table
  define('CounterpartyAndTypeHolder') {
    field 'counterpartyId',           :integer, :optional => true
    field 'nonKYC',                   :boolean
  }

  # the real trade object
  define('RefinedMetalTrade') {
    constant 'commentLength', 4000
    field 'oid',                      :integer, :identifier => true
    field 'direction',                :integer_key, :optional => true, :references => 'Direction(oid)'
    field 'audit',                    'Audit'
    field 'neptuneId',                :string
    field 'metal',                    :integer_key, :optional => true, :references => 'Metal(oid)'
    field 'ctpty_holder',             'CounterpartyAndTypeHolder'
    field 'trader',                   :integer_key, :optional => true, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'trafficOperator',          :integer_key, :optional => true, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'contractDate',             :date
    field 'submittedBy',              :integer_key, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'comments',                 :string, :max_length => RefinedMetalTrade::COMMENTLENGTH
    field 'groupCompany',             :integer_key, :optional => true, :references => 'TradeMgmt.Internal.RefinedMetal.GroupCompany(oid)'
    field 'trafficHub',               :integer_key, :optional => true, :references => 'TrafficHub(oid)'
    field 'creditDays',               :integer
    field 'quotas',                   :list, :element_type => 'TradeMgmt.Internal.RefinedMetal.Quota'
    field 'submittedDate',            :datetime
    field 'state',                    :enum, :enumerated => ['Scratch', 'Completed', 'Cancelled', 'Completing']
    field 'tolerance',                :real
    field 'sourceSystem',             :enum, :enumerated => ['Neptune', 'Titan']
  }
  
  
  
  define('RefinedMetalTradeLite') {
    field 'oid',                      :integer, :identifier => true
    field 'tradeOID',                 :integer
    field 'neptuneId',                :string
    field 'nonKYCCounterparty',       :boolean
    field 'counterpartyId',           :integer, :optional => true 
    field 'trader',                   :integer_key, :optional => true, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'trafficOperator',          :integer_key, :optional => true, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'directionId',              :integer_key, :optional => true, :references => 'Direction(oid)'
    field 'contractDate',             :date
    field 'submittedBy',              :integer_key, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'quotas',                   :integer
    field 'comments',                 :string, :max_length => 4000
    field 'groupCompanyId',           :integer, :optional => true
    field 'metalId',                  :integer, :optional => true
    field 'totalQty',                 :real, :optional => true
    field 'tolerance',                :real
    field 'tradeState',               :string
    field 'trafficHubId',             :integer, :optional => true
    field 'sourceSystem',             :string
    field 'auditModifiedById',        :integer_key, :optional => true, :references => 'TradeMgmt.Internal.Permission.User(oid)'
    field 'submittedDate',            :datetime
  }

  # as displayed in the trade blotter grid
  define('TradeBlotterRow') {

    constant 'filterByWho_MyTrades', 1
    constant 'filterByWho_AllTrades', 2
    constant 'filterByDate_Today', 1
    constant 'filterByDate_LastWeek', 2
    constant 'filterByDate_LastMonth', 3
    constant 'filterByDate_Last3Months', 4
    constant 'filterByDate_LastYear', 5
    constant 'filterByDate_All', 6

    field 'tradeOID',                 :integer
    field 'neptuneId',                :string
    field 'completeNeptuneId',        :string
    field 'nonKYCCounterparty',       :boolean
    field 'counterpartyId',           :integer, :optional => true
    field 'counterpartyName',         :string
    field 'counterpartyCode',         :string
    field 'counterpartyAddress',      :string
    field 'counterpartyPostcode',     :string, :optional => true
	field 'counterpartyPhoneNumber',  :string, :optional => true
	field 'counterpartyFaxNumber',    :string, :optional => true
	field 'counterpartyContactName',  :string, :optional => true
	field 'counterpartyAccountNumber',:string, :optional => true
    field 'traderName',               :string
    field 'trafficOperator',          :string
    field 'direction',                :boolean, :optional => true
    field 'contractDate',             :date
    field 'submittedBy',              :string
    field 'quotas',                   :integer
    field 'comments',                 :string, :max_length => 4000
    field 'groupCompany',             :string
    field 'groupCompanyId',           :integer, :optional => true
    field 'metalId',                  :integer, :optional => true
    field 'metal',                    :string
    field 'totalQty',                 :real, :optional => true
    field 'tolerance',                :real
    field 'state',                    :enum, :enumerated => ['Scratch', 'Completed', 'Cancelled', 'Completing']
    field 'trafficHubId',             :integer, :optional => true
    field 'trafficHubCode',           :string    
    field 'trafficHub',               :string
    field 'metalContractSuffix',      :string
    field 'sourceSystem',             :string
    field 'auditModifiedByName',      :string
  }

  define('RefinedMetalTradeSubmissionResult') {
    field 'trade',                    'RefinedMetalTrade'
    field 'errors',                   :list, :element_type => :string
  }
  
}
