# this is the model as used for Release 1 of RMET
in_namespace('TradeCapture.Internal.RefinedMetal') {

  define('UOM') {
    constant 'LBS', 'LBS'
    constant 'MTS', 'MTS'
    constant 'Lbs', 1
    constant 'Mts', 2
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'mappingCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  # Specifies the contractual quality of a refined metal
  define('Grade') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'metal',                  :integer
    field 'spec',                   :integer
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'registered',             :string
    field 'dutyPaid',               :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Shape') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'spec',                   :string
    field 'metal',                  :integer
    field 'name',                   :string
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Metal') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'code',                   :integer
    field 'mappingCode',            :string
    field 'contractSuffix',         :string
    field 'neptuneMaterialCode',    :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Currency') {
    constant 'AED', 'AED'
    constant 'ECB', 'ECB'
    constant 'EUR', 'EUR'
    constant 'FX1', 'FX1'
    constant 'GBP', 'GBP'
    constant 'ITL', 'ITL'
    constant 'JPY', 'JPY'
    constant 'RMB', 'RMB'
    constant 'USD', 'USD'
    constant 'ZAR', 'ZAR'
    constant 'Aed', 1 
    constant 'Ecb', 2
    constant 'Eur', 3
    constant 'Fx1', 4
    constant 'Gbp', 5
    constant 'Itl', 6
    constant 'Jpy', 7
    constant 'Rmb', 8
    constant 'Usd', 9
    constant 'Zar', 10
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'mappingCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Counterparty') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'companyCode',            :string
    field 'name',                   :string
    field 'addressId',              :integer
    field 'address',                :string
    field 'nonKYC',                 :boolean
		field 'postcode',								:string
		field 'phoneNumber',						:string
		field 'faxNumber',							:string
		field 'accountNumber',					:string
		field 'contactName',						:string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('GroupCompany') {
    constant 'AG', 'AG'
    constant 'BV', 'BV'
    constant 'HMC', 'HMC'
    constant 'MEX', 'MEX'
    constant 'PA', 'PA'
    constant 'TTS', 'TTS'
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'name',                   :string
    field 'currency',               :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Location') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'mappingCode',            :string    
    field 'code',                   :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('DestinationLocation') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'countryCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('ContractualTerms') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'mappingCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('SalesTradingRegion') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'name',                   :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('SecurityTerms') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'paymentTermsCode',       :string    
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('PaymentTerms') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'name',                   :string
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'staticRefdata',          :boolean
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Market') {
    constant 'CMX', 'CMX'
    constant 'LME', 'LME'
    constant 'NMX', 'NMX'
    constant 'SHFE', 'SHFE'
    constant 'WUXI', 'WUXI'
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'name',                   :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  # this is really qpMonth
  define('PricingOffset') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'description',            :string
    field 'offset',                 :integer
    field 'mappingCode',            :string    
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('TrafficHub') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'description',            :string
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('QPType') {
    constant 'average',  1
    constant 'unknown',  2
    constant 'fixed',    3
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean    
    field 'name',                   :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Direction') {
    constant 'purchaseDir', 1
    constant 'saleDir',     2
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'description',            :string
    field 'code',                   :string
    field 'purchase',               :boolean
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('NonKYCCounterparty') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'expired',                :boolean
    field 'code',                   :string
    field 'mappingCode',            :string
    field 'companyCode',            :string
    field 'name',                   :string
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('MarketLotSize') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'marketCode',             :string
    field 'metalCode',              :integer
    field 'currencyCode',           :string
    field 'uomCode',                :string
    field 'lotSize',                :real
    field 'spec',                   :string
    field 'expired',                :boolean
    field 'mappingCode',            :string    
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('Warehouse') {
    field 'oid',                    :integer, :identifier => true
    field 'guid',                   :guid, :business_key => true
    field 'name',                   :string
    field 'expired',                :boolean
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
  }

  define('DocumentType') {
    field 'oid',                   :integer, :identifier => true
    field 'guid',                  :guid, :business_key => true      
    field 'expired',               :boolean  
    field 'name',                  :string,      :max_length => 4000
    field 'refDataBase',           :integer_key, :references => 'RefDataBase(oid)'
  }
  
    define('GroupedReferenceData') {
         field 'uoms',            :list, :element_type => 'UOM'
         field 'metals',          :list, :element_type => 'Metal'
         field 'grades',          :list, :element_type => 'Grade'
         field 'shapes',          :list, :element_type => 'Shape'
         field 'currencies',      :list, :element_type => 'Currency'
  #       field 'counterparties',  :list, :element_type => 'Counterparty'
         field 'groupCompanies',  :list, :element_type => 'GroupCompany'
  #       field 'locations',       :list, :element_type => 'Location'
         field 'destionationLocations',  :list, :element_type => 'DestinationLocation'
         field 'contractualTerms',  :list, :element_type => 'ContractualTerms'
  #       field 'salesTradingRegions',  :list, :element_type => 'SalesTradingRegion'
         field 'securityTerms',  :list, :element_type => 'SecurityTerms'
         field 'paymentTerms',  :list, :element_type => 'PaymentTerms'
         field 'markets',  :list, :element_type => 'Market'
         field 'pricingOffsets',  :list, :element_type => 'PricingOffset'
         field 'trafficHubs',  :list, :element_type => 'TrafficHub'
         field 'qptypes',  :list, :element_type => 'QPType'
         field 'directions',  :list, :element_type => 'Direction'
         field 'marketLotSizes',  :list, :element_type => 'MarketLotSize'
         field 'DocumentTypes',  :list, :element_type => 'TradeCapture.Internal.RefinedMetal.DocumentType'
  }
}
