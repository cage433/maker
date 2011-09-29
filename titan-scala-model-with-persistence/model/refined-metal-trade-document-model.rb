expose 'ContractDocumentParameters'
expose 'ContractDocumentParameters'

# this is the model as used for Release 1 of RMET

in_namespace('TradeMgmt.Internal.RefinedMetal') {

  # used in summary and contract documents to represent a pricing fixation
  define('DocumentPricingFixationParams') {
    field 'ordinal',                   :integer
    field 'price',                     :real
    field 'currency',                  :string
    field 'currencyId',                :integer
    field 'currencyUom',               :string
    field 'currencyUomId',             :integer
    field 'brand',                     :string
    field 'quantity',                  :real
    field 'quantityUom',               :string
    field 'quantityUomId',             :integer
  }

  # used in document summaries to represent a hedge request
  define('DocumentHedgeRequestParams') {
    field 'ordinal',                   :integer
    field 'market',                    :string
    field 'marketId',                  :integer
    field 'contractMonthName',         :string
    field 'price',                     :real
    field 'currency',                  :string
    field 'currencyId',                :integer
    field 'currencyUom',               :string
    field 'currencyUomId',             :integer
    field 'lots',                      :integer
    field 'direction',                 :string
    field 'hrNumber',                  :string
  }
  
  define('DocumentFxHedgeRequestParams', :extends => 'DocumentHedgeRequestParams') {
    field 'fxRate',                    :real
    field 'fxCurrency',                :string
    field 'fxUom',                     :string
  }

  # pricing specification list
  define('DocumentPricingSpecParams') {
    field 'ordinal',                   :integer
    field 'pricingFixations',          :list, :element_type => 'DocumentPricingFixationParams'
    field 'hedgeRequests',             :list, :element_type => 'DocumentHedgeRequestParams'
    field 'comments',                  :string
    field 'premiumComments',           :string
  }

  define('ContractDocumentQuotaParameters') {
    field 'ordinal',                   :integer
    field 'quantity',                  :real
    field 'uom',                       :string
    field 'estimatedSales',            :monthYear
    field 'shipment',                  :monthYear
    field 'vatInvoiceDate',            :date, :optional => true
    field 'qpTypeId',                  :integer
    field 'presentedDocumentName',     :string
    field 'presentedDocumentIsCopy',   :boolean
    field 'numberOfDeferredCreditDays',:integer
    field 'pricingSpecParams',         :list, :element_type => 'DocumentPricingSpecParams'
  }

  define('ContractDocumentParameters') {
    field 'draft',                     :boolean
    field 'neptuneId',                 :string
    field 'counterpartyName',          :string
    field 'nonKYCCounterparty',        :boolean
    field 'counterpartyAddress',       :string
    field 'counterpartyPostcode',      :string
    field 'counterpartyPhoneNumber',   :string
    field 'counterpartyFaxNumber',     :string
    field 'counterpartyContactName',   :string
    field 'counterpartyAccountNumber', :string
    field 'traderName',                :string
    field 'purchase',                  :boolean
    field 'contractDate',              :date
    field 'groupCompany',              :string
    field 'hub',                       :string
    field 'totalQty',                  :real
    field 'uom',                       :string
    field 'tolerance',                 :real
    field 'comments',                  :string
    field 'metal',                     :string
    field 'grade',                     :string
    field 'shape',                     :string
    field 'gradeComments',             :string
    field 'contractualLocation',       :string
    field 'contractualTerms',          :string
    field 'paymentTerms',              :string
    field 'securityTerms',             :string
    field 'market',                    :string
    field 'currency',                  :string
    field 'premium',                   :real
    field 'premiumCcy',                :string
    field 'premiumUom',                :string
    field 'qpType',                    :string
    field 'pricingOffset',             :string
    field 'quotas',                    :list, :element_type => 'ContractDocumentQuotaParameters'
    field 'metalContractSuffix',       :string
    field 'hubCode',                   :string
    field 'sourceSystem',              :string
    field 'auditModifiedByName',       :string
  }

  # container for contract document parameters or a missing translations list 
  define('CDocParsResp') {
    field 'cdocResult',                :boolean
    field 'cDocParameters',            'ContractDocumentParameters'
    field 'reqTranslations',           list('RichTranslation')
  }
  
  define('SummaryDocumentQuotaParameters') {
    field 'ordinal',                   :integer
    field 'quantity',                  :real, :optional => true
    field 'uom',                       :string
    field 'deliveryMonth',             :string
    field 'deliveryYear',              :string
    field 'shipmentMonth',             :string
    field 'shipmentYear',              :string
	field 'vatInvoiceDate',            :date, :optional => true
    field 'vatInvoiceType',            :enum, :enumerated => ['Issued', 'Received'], :optional => true
    field 'qpTypeId',                  :integer
    field 'presentedDocumentName',     :string
    field 'presentedDocumentIsCopy',   :boolean
    field 'numberOfDeferredCreditDays',:integer
    field 'pricingSpecParams',         :list, :element_type => 'DocumentPricingSpecParams'
  }
    
  # returned by the trade repository for Murdoch/Thunderhead to generate trade summary document
  define('SummaryDocumentParameters') {
    field 'neptuneId',                 :string
    field 'contractRef',               :string
    field 'purchase',                  :boolean, :optional => true
    field 'contractDate',              :date
    field 'groupCompany',              :string
    field 'hub',                       :string
    field 'traderName',                :string
    field 'trafficOperator',           :string
    field 'counterpartyName',          :string
    field 'metal',                     :string
    field 'totalQty',                  :real, :optional => true
    field 'uom',                       :string
    field 'grade',                     :string
    field 'shape',                     :string
    field 'gradeComments',             :string
    field 'brand',                     :string
    field 'contractualLocation',       :string
    field 'contractualTerms',          :string
    field 'destinationLocation',       :string
    field 'destinationTerms',          :string
    field 'quotaCount',                :integer
    field 'premium',                   :real, :optional => true
    field 'premiumCcy',                :string
    field 'premiumUom',                :string
    field 'premiumComments',           :string
    field 'market',                    :string
    field 'currency',                  :string
    field 'qpType',                    :string
    field 'pricingOffset',             :string
    field 'paymentTerms',              :string
    field 'securityTerms',             :string
    field 'quotas',                    :list, :element_type => 'SummaryDocumentQuotaParameters'
    field 'sourceSystem',              :string
    field 'auditModifiedByName',       :string
  }

  # returned by the trade repository for Murdoch/Thunderhead to generate trade summary document
  define('DocumentStorageParameters') {
    field 'groupCompany',              :string
    field 'purchaseOrSale',            :string
    field 'languageCode',              :string
    field 'counterparty',              :string
    field 'trafficHub',                :string
    field 'metal',                     :string
    field 'trader',                    :string
    field 'neptuneId',                 :string
    field 'contractNumber',			   :string
  }
}

