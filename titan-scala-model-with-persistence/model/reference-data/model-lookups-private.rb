# this is the model for Reference Data Project
in_namespace('inner.ReferenceData') {


  define('RdLookupType') {




    constant 'AccountCodeType', 'ACCO' 
    constant 'AddressType', 'ATYPE' 
    constant 'ApprovedStatus', 'ASTAT' 
    constant 'BankAccountFlag', 'BACFL' 
    constant 'Commodity', 'RMET' 
    constant 'ContactRole', 'CROLE' 
    constant 'Continent', 'CONT' 
    constant 'CostCodeDirection', 'CCDI' 
    constant 'CostCodeFlag', 'CCFL' 
    constant 'CounterpartyFlag', 'FLAGS' 
    constant 'CreditLimitBehaviour', 'CLTB' 
    constant 'CreditLimitCategory', 'CLC' 
    constant 'DefaultBankCharge', 'DBCH'
    constant 'DerivativeRight', 'DERRI'
    constant 'DocumentType', 'DCTYP'
    constant 'ExerciseType', 'EXTP'
    constant 'Font', 'FONTS'
    constant 'FundamentalUOM', 'UOM'
    constant 'FXRule', 'FXR'
    constant 'GeographicalGrouping', 'GEOG' 
    constant 'Grade', 'RGRAD' 
    constant 'HeaderFooter', 'HFOOT' 
    constant 'ImagePosition', 'IMGPS'
    constant 'InterestRateType', 'INTP'
    constant 'InterestType', 'LEIT' 
    constant 'InternationalOrganisation', 'ORG' 
    constant 'BankFlag', 'BNKFL' 
    constant 'LegalEntityRole', 'LROLE' 
    constant 'Month', 'MONTH' 
    constant 'NoteCategory', 'CATG' 
    constant 'OrderBasis', 'ORBAS' 
    constant 'OrderOptionContractType', 'COTYP' 
    constant 'OrderOptionType', 'OPTYP' 
    constant 'OrderRequestType', 'ORTYP' 
    constant 'OrderValidTime', 'OVTIM'
    constant 'PaymentTerm', 'PAYTR'
    constant 'QPType', 'QPTYP'
    constant 'RatingsAgent', 'RTAG' 
    constant 'Region', 'REGN' 
    constant 'RegistredOffice', 'REGOF' 
    constant 'RiskOffice', 'RISK' 
    constant 'Secured', 'SECST' 
    constant 'TransportType', 'TRTYP' 
    constant 'VatCode', 'VAT' 
    constant 'VatInvoice', 'TAXCD'
    constant 'Vessel', 'VESSL'

    field 'id',            :string

    field 'shortCode',     :string
    field 'description',   :string
    field 'modifiedBy',    :string
  }


  define('RdStatus') {
    field 'id',            :string
    mixin('MainVersionable')

    field 'shortCode',     :string
    field 'description',   :string
  }


  define('RdLanguage') {
    constant 'langEn', 'EN'

    field 'id',            :string
    mixin('MainVersionable')

    field 'shortCode',     :string
    field 'description',   :string
  }


  define('RdTranslation') {
    field 'id',            :string
    mixin('ChildVersionable')

    field 'caption',       :string
    field 'fieldName',     :string

    field 'languageId',    :string
  }


  define('RdBusinessLine') {
    field 'id',            :string
    mixin('MainVersionable')

    field 'shortCode',     :string
    field 'description',   :string
  }


  define('RdLookup') {
    constant 'descriptionFieldName', 'CL_DESC'

    field 'id',            :string
    mixin('MainVersionable')

    field 'shortCode',     :string
    field 'description',   :string
    field 'rank',          :integer, :optional => true

    field 'childTypeCode', :string
    field 'childTypeRef',  'RdLookupType'
    field 'typeRef',       'RdLookupType'
    field 'statusId',      :string

    field 'translations',  :list, :element_type => 'RdTranslation'

    field 'businessLinesIds', :list, :element_type => :string
  }

# International Organisation - object that represents International Organisation. Extends Lookup
  define('RdInternationalOrg', :extends => 'RdLookup') {}

# Political Unit Group - object that represents Political Unit Group. Extends Lookup
  define('RdPoliticalUnitGroup', :extends => 'RdLookup') {}

# VAT - object that represents VAT. Extends Lookup
  define('RdVatCode', :extends => 'RdLookup') {
  constant 'classShortCode', 'VAT'
  }

# Approved status - object that represents Approved Status. Extends Lookup
  define('RdApprovedStatus', :extends => 'RdLookup') {}

# Continent - object that represents Continent. Extends Lookup
  define('RdContinent', :extends => 'RdPoliticalUnitGroup') {}

# Region - object that represents Region. Extends Lookup
  define('RdRegion', :extends => 'RdPoliticalUnitGroup') {}

# Rating Agency - object that represents Rating Agency. Extends Lookup
  define('RdRatingAgency', :extends => 'RdLookup') {
    constant 'SP', "SP"
    constant 'Moodys', "MDY"
    constant 'Trafigura', "TRAF"
  }

# Company Role - object that represents Company Role. Extends Lookup
  define('RdCompanyRole', :extends => 'RdLookup') {}

# Transport Type entity, extends Lookup
  define('RdTransportType', :extends => 'RdLookup') { }

# Order Request Type entity, extends Lookup
  define('RdOrderRequestType', :extends => 'RdLookup') { 
  constant 'classShortCode', 'ORTYP'
  }

# Order Valid Time entity, extends Lookup
  define('RdOrderValidTime', :extends => 'RdLookup') { 
  constant 'classShortCode', 'OVTIM'
  }

# Order Basis entity, extends Lookup
  define('RdOrderBasis', :extends => 'RdLookup') { 
  constant 'classShortCode', 'ORBAS'
  }

# Option Type entity, extends Lookup
  define('RdOrderOptionType', :extends => 'RdLookup') {
  constant 'classShortCode', 'OPTYP'
  }

# Option Contract Type entity, extends Lookup
  define('RdOrderOptionContractType', :extends => 'RdLookup') { 
  constant 'classShortCode', 'COTYP'
  }
  # Vessel, extends Lookup
  define('RdVessel', :extends => 'RdLookup') {}

  # DerivativeRight, extends Lookup
  define('RdDerivativeRight', :extends => 'RdLookup') {}

  # ExerciseType, extends Lookup
  define('RdExerciseType', :extends => 'RdLookup') {}

  # FXRule, extends Lookup
  define('RdFXRule', :extends => 'RdLookup') {}

  # InterestRateType, extends Lookup
  define('RdInterestRateType', :extends => 'RdLookup') {}

  # FundamentalUOM, extends Lookup
  define('RdFundamentalUOM', :extends => 'RdLookup') {}

  # QPType, extends Lookup
  define('RdQPType', :extends => 'RdLookup') {}

  # PaymentTerm, extends Lookup
  define('RdPaymentTerm', :extends => 'RdLookup') {}

  # DocumentType, extends Lookup
  define('RdDocumentType', :extends => 'RdLookup') {}

}

in_namespace('inner.ReferenceData.Accounting') {
=begin
    We might need to to extend this to be able to provide a specific type
    however it's unknown yet.
=end
  define('RdAccountCodeType', :extends => 'RdLookup') {
      # CODE_SETS.SHORT_CODE   ::  ACCO
  # CODE_SETS.SET_DESC     ::  Account codes
  # CODE_LOOKUPS.CL_VALUE  ::  SCRED, SDEBT
  # CODE_LOOKUPS.CL_DESC   ::  Sun Credit Account, Sun Debtor Account
  }

}

in_namespace('inner.ReferenceData.LegalEntities') {

# Enumerated types

# Interest Types
  define('RdInterestType', :extends => 'RdLookup') {}

  # RdRiskOffice
  define('RdRiskOffice', :extends => 'RdLookup') {}

  # RdCategory
  define('RdCategory', :extends => 'RdLookup') {}

  # RdCompany flags
  define('RdFlag', :extends => 'RdLookup') {}

  # Default Bank Charges three values from CODE_LOOKUP table (Trafigura(OUR), Counterparties(BEN), Shared(SHA))
  define('RdDefBankCharge', :extends => 'RdLookup') {}

  define('RdContactRole', :extends => 'RdLookup') {}

  define('RdRegistredOffice', :extends => 'RdLookup') {}

  define('RdMonth', :extends => 'RdLookup') {}

  define('RdBankAccountFlag', :extends => 'RdLookup') {}

  define('RdHeaderFooter', :extends => 'RdLookup') {}

  define('RdFont', :extends => 'RdLookup') {}

  define('RdImagePosition', :extends => 'RdLookup') {}

  define('RdVatInvoice', :extends => 'RdLookup') {}

  define('RdSecured', :extends => 'RdLookup') {}

  define('RdLegalEntityRole', :extends => 'RdLookup') {
  constant 'classShortCode', 'LROLE'
  }

  define('RdLeBankFlag', :extends => 'RdLookup') { }

}

# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.Locations') {

  define('RdLogisticsLocationType', :extends => 'RdLookup') {
  }

  define('RdAddressType', :extends => 'RdLookup') {
  }

  define('RdFacilityType', :extends => 'RdLookup') {
  constant 'classShortCode', 'LOGL'  
  }

}

in_namespace('inner.ReferenceData.CostCodes') {

  # COIDirection - object that represents COIDirection. COIDirection
  define('RdCostCodeDirection', :extends => 'RdLookup') { 
  constant 'classShortCode', 'CCDI'
  }

  # RdCoiFlag - object that represents RdCoiFlag. flags for Cost Codes
  define('RdCostCodeFlag', :extends => 'RdLookup') { }
 
                                         
}

in_namespace('inner.ReferenceData.CreditLimit') {

  # CreditLimitCategory - object that represents RdCreditLimitCategory
  define('RdCreditLimitCategory', :extends => 'RdLookup') { }

  # CreditLimitBehaviour - object that represents CreditLimitBehaviour
  define('RdCreditLimitBehaviour', :extends => 'RdLookup') { }
 
                                         
}




