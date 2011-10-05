import 'tc-ref-data-overlay.rb'

in_namespace('Shared.Events') {
  define('Event', :hibernate_persist => false)
  define('Header', :hibernate_persist => false)
  define('Body', :hibernate_persist => false)
  define('Payload', :hibernate_persist => false)
  define('EventKey', :hibernate_persist => false)
}

in_namespace('EDM.shared.types') {
  define('DateSpec', :hibernate_persist => false)
  define('Date', :hibernate_persist => false)
  define('Tolerance', :hibernate_persist => false)
  define('QuantityTolerance', :hibernate_persist => false)
  define('PercentageTolerance', :hibernate_persist => false)
  define('UOM', :hibernate_persist => false)
  define('FundamentalUOM', :hibernate_persist => false)
  define('CompoundUOM', :hibernate_persist => false)
  define('Currency', :hibernate_persist => false)
}

in_namespace('EDM.PricingSchedules') {
  define('PricingSchedule', :hibernate_sequence_name => 'PricingSchedule_seq')
}

in_namespace('EDM.PaymentSpecs') {
  define('PaymentSpecification', :hibernate_persist => false ) 
  define('PresentedDocument', :hibernate_persist => false ) 
}

in_namespace('EDM.PhysicalTradeSpecs') {
  define('PricingSpecification', :hibernate_persist => false)
  define('EDMHedgeRequest', :hibernate_persist => false)
  define('FxOverride', :hibernate_persist => false)
  define('FixedPriceComponent', :hibernate_persist => false)
  define('FixedPricingSpecification', :hibernate_persist => false)
  define('UnknownPricingFixation', :hibernate_persist => false)
  define('UnknownPricingSpecification', :hibernate_persist => false)
  define('AveragePricingFixation', :hibernate_persist => false)
  define('AveragePricingSpecification', :hibernate_persist => false)
  define('MonthAveragePricingSpecification', :hibernate_persist => false)
  define('PartialAverageDayQuantity', :hibernate_persist => false)
  define('PartialAveragePricingSpecification', :hibernate_persist => false)
  define('PriceSpecificationWeighting', :hibernate_persist => false)
  define('WeightedPricingSpecification', :hibernate_persist => false)
  define('OptionalPricingSpecification', :hibernate_persist => false)
}

in_namespace('EDM.TradeService') {
  define('TradeSubmissionResult', :hibernate_persist => false)     
}

in_namespace('EDM.Trades') {
  define('Trade', :hibernate_persist => false)     
  define('PaperTrade', :hibernate_persist => false)     
  define('PhysicalTrade', :hibernate_persist => false)     
  define('OTCTrade', :hibernate_persist => false)     
  define('Transaction', :hibernate_persist => false)     
  define('ContractualAgreement', :hibernate_persist => false)     
  define('TradeSpec', :hibernate_persist => false)     
  define('TradeContractTerms', :hibernate_persist => false)     
  define('PhysicalTradeContractTerms', :hibernate_persist => false)     
  define('CounterpartyAndType', :hibernate_persist => false)     
  define('Contact', :hibernate_persist => false)     
}

in_namespace('EDM.MaterialSpecification') {
  define('MaterialSpec', :hibernate_persist => false)
  define('ProductSpec', :hibernate_persist => false)
  define('CommoditySpec', :hibernate_persist => false)
  define('RefinedMetalSpec', :hibernate_persist => false)
}

in_namespace('EDM.PhysicalTradeSpecs') {
  define('DeliveryLocation', :hibernate_persist => false)
  define('DeliverySpec', :hibernate_persist => false)
  define('QuotaDetails', :hibernate_persist => false)
  define('PhysicalTradeQuota', :hibernate_persist => false)
  define('PhysicalTradeSpec', :hibernate_persist => false)
}

in_namespace('TradeCapture.Internal.RefinedMetalDocumentService') {
  define('ReviewCase', :hibernate_persist => false)
  define('DocumentDef', :hibernate_persist => false)
  define('ReviewCaseDef', :hibernate_persist => false)
  define('TranslationResponse', :hibernate_persist => false)
  define('DocumentResponse', :hibernate_persist => false)
}

in_namespace('TradeCapture.Internal.RefinedMetalTradeService') {
  define('NeptuneResult', :hibernate_persist => false)
  define('NeptuneLoginResult', :hibernate_persist => false)
  define('SyncResult', :hibernate_persist => false)
  define('SearchResult', :hibernate_persist => false)
  define('GetTradeWithVersionsResult', :hibernate_persist => false)
}

in_namespace('TradeCapture.Internal.RefinedMetal') {
  define('ContractDocumentQuotaParameters', :hibernate_persist => false)
  define('DocumentPricingFixationParams', :hibernate_persist => false)
  define('DocumentHedgeRequestParams', :hibernate_persist => false)
  define('DocumentPricingSpecParams', :hibernate_persist => false)
  define('GroupedReferenceData', :hibernate_persist => false)

  define('ContractDocumentParameters', :hibernate_persist => false) {
    field 'quotas', :hibernate_bidirectional => true
  }

  define('SummaryDocumentParameters', :hibernate_persist => false) {
    field 'quotas', :hibernate_bidirectional => true
  }

  define('RichTranslation', :hibernate_persist => false)
  define('Translations', :hibernate_persist => false)
  define('CDocParsResp', :hibernate_persist => false)

  define('DocumentStorageParameters', :hibernate_persist => false)
  define('AmendmentParameters', :hibernate_persist => false)
  define('TradeBlotterQuotaRow', :hibernate_persist => false)
  define('TradeBlotterRow', :hibernate_persist => false)
  define('RefinedMetalTradeSubmissionResult', :hibernate_persist => false)     

  
  define('SummaryDocumentQuotaParameters', :hibernate_persist => false)
  define('DocumentFxHedgeRequestParams', :hibernate_persist => false)

  define('RefDataBase', :hibernate_sequence_name => 'RefDataBase_seq')

  define('Language', :hibernate_sequence_name => 'Language_seq')

  define('Translation', :hibernate_sequence_name => 'Translation_seq')  

  define('TradeWarning', :hibernate_sequence_name => 'TradeWarning_seq')

  define('RefinedMetalTrade', :hibernate_sequence_name => 'RefinedMetalTrade_seq') {
    field 'docInfo', :hibernate_bidirectional => true
    field 'amendmentInfo', :hibernate_bidirectional => true
    field 'quotas', :hibernate_bidirectional => true
  }
   
  
  define('RefinedMetalTradeLite', :persistence_name => 'TRADE_BLOTTER_ROW_VIEW') {  
  }

  define('Quota', :hibernate_sequence_name => 'Quota_seq') {
    field 'pricingSpecs', :hibernate_bidirectional => true
    field 'index', :persistence_name => 'q_index'
  }
  
  
  define('DocLangDetails', :hibernate_sequence_name => 'DocLangDetails_seq') 
  define('AmendmentLangDetails', :hibernate_sequence_name => 'AmendmentLangDetails_seq') 
  

  define('PaymentSpec', :hibernate_sequence_name => 'PaymentSpec_seq') {
    field 'sellerChargesRechargedToBuyer',     :persistence_name => 'sellerChargRechargBuyer'
    field 'presentedDocuments',                :hibernate_bidirectional => false, :persistence_name => 'presentDocs'
    field 'paymentDates',                      :hibernate_bidirectional => false
    field 'paymentMethods',                    :hibernate_bidirectional => false, :persistence_name => 'payMethods'
    field 'gt',                         	   :persistence_name => 'gt'
  }

   define('PaymentMethod', :hibernate_sequence_name =>       'PaymentMethod_seq') 
   define('TelegraphicTransfer', :hibernate_sequence_name => 'TelegraphicTransfer_seq') 
   define('LetterOfCredit', :hibernate_sequence_name =>       'LetterOfCredit_seq') 
   define('OpenAccount', :hibernate_sequence_name =>          'OpenAccount_seq') 
   define('PaymentDate', :hibernate_sequence_name =>          'PaymentDate_seq') 
   define('PresentedDocument', :hibernate_sequence_name =>    'PresentedDocument_seq') 
}

in_namespace('EDM.FPC') {
  # Trade Pricing spec
  define('PricingFixation', :hibernate_sequence_name => 'PricingFixation_seq')
  define('NeptunePricingFixation', :hibernate_sequence_name => 'NeptunePricingFixation_seq')
  define('HedgeRequest', :hibernate_sequence_name => 'HedgeRequest_seq')
  define('FxHedgeRequest', :hibernate_sequence_name => 'FxHedgeRequest_seq')
  define('PhysicalHedgeRequest', :hibernate_sequence_name => 'PhysicalHedgeRequest_seq')
  define('PricingSpec', :hibernate_sequence_name => 'PricingSpec_seq') {
    field 'pricingFixations', :hibernate_bidirectional => true
    field 'neptunePricingFixations', :hibernate_bidirectional => true
    field 'hedgeRequests', :hibernate_bidirectional => true
    field 'neptuneHedgeRequests', :hibernate_bidirectional => true
  }
   
  define('UnkPricingSpec', :hibernate_sequence_name => 'UnkPricingSpec_seq')
  define('PartialAvePricingSpec', :hibernate_sequence_name => 'PartialAvePricingSpec_seq')
  define('MonthAveragePricingSpec', :hibernate_sequence_name => 'MonthAveragePricingSpec_seq')

}

in_namespace('EDM.PaperTradeSpecs') {
  define('PaperTradeSpec', :hibernate_persist => false)
}

in_namespace('TradeCapture.Internal.PermissionService') {
  define('GroupRoles', :hibernate_sequence_name => 'GroupRoles_seq')
  define('Group', :hibernate_sequence_name => 'GroupX_seq')
  define('Role', :hibernate_sequence_name => 'RoleX_seq')
  define('UserGroups', :hibernate_sequence_name => 'UserGroups_seq')
  define('UserRoles', :hibernate_sequence_name => 'UserRoles_seq')
  define('User', :hibernate_sequence_name => 'UserX_seq') {
     field 'effectivePermissions', :hibernate_persist => false
     field 'effectivePermissionsSet', :hibernate_persist => false
  }
  define('TrafficOperatorAndTradersForGC', :hibernate_persist => false)
  define('Permission', :hibernate_persist => false)
  define('PermissionResult', :hibernate_persist => false)
  define('PermissionRole', :hibernate_persist => false)
}

#in_namespace('CostsAndIncomes.Internal.CostsRepository', :hibernate_persist => false)
