in_namespace('TradeMgmt.Internal.RefinedMetalReferenceData') {

  service('UOMService') {
    operation('GetUOMs', :returns => list('TradeMgmt.Internal.RefinedMetal.UOM')) {
    }
  }

  service('MarketLotSizeService') {
    operation('GetMarketLotSizes', :returns => list('MarketLotSize')) {
    }
  }

  service('WarehouseService') {
    operation('GetWarehouses', :returns => list('Warehouse')) {
    }
  }

  service('DocumentTypeService') {
    operation('GetDocumentTypes', :returns => list('TradeMgmt.Internal.RefinedMetal.DocumentType')) {
    }
  }

  service('GradeService') {
    operation('GetGrades', :returns => list('Grade')) { }
    operation('GetGradesForMetal', :returns => list('Grade')) {
      parameter 'metal', :Metal
    }
  }
  
  
  service('GroupedReferenceDataService') {
    operation('GetAll', :returns => 'GroupedReferenceData') { }  
  }

  service('ShapeService') {
    operation('GetShapes', :returns => list('Shape')) { }
    operation('GetShapesForMetal', :returns => list('Shape')) {
      parameter 'metal', :Metal
    }
    operation('GetShapesForMetalAndGrade', :returns => list('Shape')) {
      parameter 'metal', :Metal
      parameter 'grade', :Grade
    }
  }

  service('QualityService') {
    operation('GetQualities', :returns => list('Quality')) {
    }
  }

  service('MetalService') {
    operation('GetMetals', :returns => list('Metal')) {
    }
  }

  service('CurrencyService') {
    operation('GetCurrencies', :returns => list('TradeMgmt.Internal.RefinedMetal.Currency')) {
    }
  }

  service('CounterpartyService') {
    operation('CreateNonKYCCounterparty', :returns => :integer) {
      parameter 'nonKYCCounterParty', :NonKYCCounterparty
    }
    operation('UpdateNonKYCCounterparty', :returns => :boolean) {
      parameter 'nonKYCCounterParty', :NonKYCCounterparty
    }
    operation('GetNonKYCCounterparty', :returns => :NonKYCCounterparty) {
      parameter 'oid', :integer
    }
    operation('CheckNonKYCCounterpartyExists', :returns => :boolean) {
      parameter 'oid', :integer
    }
    operation('CheckNonKYCCounterpartyExistsForGroupCompany', :returns => :boolean) {
      parameter 'oid', :integer
      parameter 'groupCompany', :'TradeMgmt.Internal.RefinedMetal.GroupCompany'
    }
    operation('GetCounterpartyForCode', :returns => list('Counterparty')) {
      parameter 'counterpartyCode', :string
    }
    operation('GetCounterparties', :returns => list('Counterparty')) {
      parameter 'includeTemporary', :boolean     
    }
    operation('GetCounterpartiesForGroupCompany', :returns => list('Counterparty')) {
      parameter 'company', 'TradeMgmt.Internal.RefinedMetal.GroupCompany'
    }
  }

  service('GroupCompanyService') {
    operation('GetGroupCompanies', :returns => list('TradeMgmt.Internal.RefinedMetal.GroupCompany')) {
    }
  }

  service('LocationService') {
    operation('GetLocations', :returns => list('Location')) {
    }
  }
  service('DestinationLocationService') {
    operation('GetDestinationLocations', :returns => list('DestinationLocation')) {
    }
  }

  service('ContractualTermsService') {
    operation('GetContractualTerms', :returns => list('ContractualTerms')) {
    }
  }

  service('SalesTradingRegionService') {
    operation('GetSalesTradingRegions', :returns => list('SalesTradingRegion')) { }
    operation('GetSalesTradingRegionsForLocation', :returns => list('SalesTradingRegion')) { 
      parameter 'location', 'DestinationLocation'
    }
  }

  service('SecurityTermsService') {
    operation('GetSecurityTerms', :returns => list('SecurityTerms')) {
    }
    operation('GetSecurityTermsForPaymentTerms', :returns => list('SecurityTerms')) {
      parameter 'paymentTerms', 'PaymentTerms'    
    }
  }

  service('PaymentTermsService') {
    operation('GetPaymentTerms', :returns => list('PaymentTerms')) {
    }
  }

  service('MarketService') {
    operation('GetMarkets', :returns => list('Market')) {
    }
  }

  service('QPTypeService') {
    operation('GetQPTypes', :returns => list('TradeMgmt.Internal.RefinedMetal.QPType')) {
    }
  }

  service('DirectionService') {
    operation('GetDirections', :returns => list('Direction')) {
    }
  }

  service('TrafficHubService') {
    operation('GetTrafficHubs', :returns => list('TrafficHub')) {
    }
  }
  service('PricingOffsetService') {
    operation('GetPricingOffsets', :returns => list('PricingOffset')) {
    }
  }


  ## translation management
  service('TranslationsService') {

    # get all translations
    operation('GetAllTranslations', :returns => list('TradeMgmt.Internal.RefinedMetal.Translation')) {
    }

    # get all translations for a given language
    operation('GetTranslationsForLanguage', :returns => list('TradeMgmt.Internal.RefinedMetal.Translation')) {
      parameter 'languageCode', :string
    }

    # create / update translations
    operation('SaveTranslations', :returns => list('TradeMgmt.Internal.RefinedMetal.Translation')) {
      parameter 'translations', list('TradeMgmt.Internal.RefinedMetal.Translation')
    }

    # delete all translations from the given list
    operation('DeleteTranslations', :returns => :boolean) {
      parameter 'translations', list(:integer)
    }

    # get all available languages
    operation('GetAllLanguages', :returns => list("TradeMgmt.Internal.RefinedMetal.Language")) {
    }

    # translation operation, return lists of translated items / missing translations
    operation('GetTranslations', :returns => 'TradeMgmt.Internal.RefinedMetal.Translations') {
      parameter 'languageCode', :string
      parameter 'regTranslations', list('TradeMgmt.Internal.RefinedMetal.Translation')
    }
  }
}
