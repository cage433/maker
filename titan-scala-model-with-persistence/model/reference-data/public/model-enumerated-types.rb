# EDM package EDM.Enterprise.Common Domains.Reference Data.Enumerated Types
in_namespace('commondomains.referencedata.enumeratedtypes') {
=begin
  Base class for all the classes which are enumerations. This class
  is basically maps to the CODE_LOOKUPS table. The exact type is mapped
  via the CODE_SETS table. All entities based on this class might use
  the 'shortCode' field in order to identify the record in the database.
=end
  define('EnumeratedItem', :extends => 'Entity') {
      # CL_VALUE - The unique identifier for the record
    field 'shortCode', :string
    # CL_DESC - Th human readable description in English
    field 'description', :string
    # RANK - The rank of the record
    field 'rank', :integer, :optional => true
=begin
    VER_START + VER_END - A date range when the record is active. 
    The end date can be null.
=end
    field 'effectiveDateRange', 'commontypes.dataspecifications.DateRange'
  }


  # This entity maps to the LANGUAGES table
  define('Language', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to the LNG_SHORT_CODE column
=end
    field 'shortCode', :string
    # This field maps to the LNG_DESC column
    field 'description', :string
  }

  # This entity maps to the LANGUAGE_VERSIONS table
  define('Translation', :extends => 'Entity') {
      # This field maps to the LANG_ID column
    field 'languageGUID', :string
    # This field maps to the REC_ID column
    field 'recordGUID', :string
    # This field maps to the FIELD_NAME column
    field 'fieldName', :string
    # This field maps to the TRANS column
    field 'translationx', :string
  }

=begin
  VAT - object that represents VAT. Extends Enumerated item
  maps to the CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('VatCode', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }

=begin
  This enumerates the most basic dimensions of measurement,
  such as weight, length, currency, etc.
  but without specifying the precise unit (e.g. tons, feet, dollars, etc.).
  Compound dimensions such as price/ton etc. are not included.
=end
  define('FundamentalUnitType', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }

=begin
  ConcentrationLevelType - object that represents Concentration Level Type. Extends Enumerated item
  maps to the CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('ConcentrationLevelType', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   :: CLTYP
  # CODE_SETS.SET_DESC     ::
  }

=begin
  Refined metal grade - object that represents a brand which can be
  associated to a refined metal. Extends Enumerated item maps to the
  CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('RefinedMetalGrade', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::  RGRAD
  # CODE_SETS.SET_DESC     ::
  }

=begin
  Commodities - object that represents a commodity which can be
  associated to a refined metal. Extends Enumerated item maps to the
  CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('Commodity', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::  RMET
  # CODE_SETS.SET_DESC     ::
  }

  # Refined Metal shape, maps to SHAPES table
  define('RefinedMetalShape', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    SHAPE_SHORT_CODE column
=end
    field 'shortCode', :string
    # SHAPE_DESCRIPTION column
    field 'description', :string
  }

  # Refined Metal shape, maps to BRANDS table
  define('RefinedMetalBrand', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    BRAND_SHORT_CODE column
=end
    field 'shortCode', :string
    # BRAND_DESCRIPTION column
    field 'description', :string
    # LME_REGISTERABLE column
    field 'lmeRegisterable', :boolean
  }

=begin
  This entity maps to LOCATION_FUNCTION_TYPES table
=end
  define('LocationFunction', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to LOCATION_FUNCTION_TYPES.FUNCTION_CODE
=end
    field 'shortCode', :string
    # This field maps to LOCATION_FUNCTION_TYPES.FUNCTION_DESC
    field 'description', :string
  }

=begin
  Company Role - object that represents a role a legal entity can be
  assigned. Extends Enumerated item maps to the CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('CompanyRole', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }

  # This entity maps to the BUSINESS_LINES table
  define('BusinessLine', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }

=begin
  Numeric Type - object that represents any numeric types.
  Extends Enumerated item maps to the CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('NumericType', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }


=begin
  Cost & Incomes Direction - object that represents a direction of transaction.
  Extends Enumerated item maps to the CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('COIDirection', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   :: CCDI
  # CODE_SETS.SET_DESC     :: 'Cost Codes Directions'
  }

=begin
  Cost Or Income Granularity - object that represents the enumeration of CoI granularity.
  Enumerated item which maps to the COST_CODE_GRANULARITY_LEVELS table
=end
  define('COIGranularity', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to the SHORT_CODE column
=end
    field 'shortCode', :string
    # This field maps to the DESCRIPTION column
    field 'description', :string
=begin
    This field maps to the STATUSES table
    where STATUSES.ID = COST_CODE_GRANULARITY_LEVELS.STATUS_ID
=end
    field 'status', 'Status'
=begin
    This field maps to the VER_START, VER_END fields
=end
    field 'effectiveDateRange', 'commontypes.dataspecifications.DateRange'
=begin
    This field maps to the COST_CODE_GRANULARITY_LEVELS table
    where COST_CODE_GRANULARITY_LEVELS.ID = COST_CODE_GRANULARITY_LEVELS.PARENT_GRANULARITY_LEVEL_ID
=end
    field 'parent', 'COIGranularity'
  }

=begin
  Cost Or Income Type - object that represents the enumeration of CoI type.
  Enumerated item which maps to the COST_CODE_TYPES table
=end
  define('COIType', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to the SHORT_CODE column
=end
    field 'shortCode', :string
    # This field maps to the DESCRIPTION column
    field 'description', :string
=begin
    This field maps to the STATUSES table
    where STATUSES.ID = COST_CODE_TYPES.STATUS_ID
=end
    field 'status', 'Status'
=begin
    This field maps to the VER_START, VER_END fields
=end
    field 'effectiveDateRange', 'commontypes.dataspecifications.DateRange'
=begin
    This field maps to the COST_CODE_TYPES table
    where COST_CODE_TYPES.ID = COST_CODE_TYPES.PARENT_COST_CODE_TYPE_ID
=end
    field 'parent', 'COIType'
  }

=begin
  Cost Or Income Code - object that represents the enumeration of CoI code.
  Enumerated item which maps to the COST_CODES table
=end
  define('COICode', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to the SHORT_CODE column
=end
    field 'shortCode', :string
    # This field maps to the DESCRIPTION column
    field 'description', :string
=begin
    This field maps to the STATUSES table
    where STATUSES.ID = COST_CODES.STATUS_ID
=end
    field 'status', 'Status'
=begin
    This field maps to the VER_START, VER_END fields
=end
    field 'effectiveDateRange', 'commontypes.dataspecifications.DateRange'
=begin
    This field maps to the COST_CODE_TYPES table
    where COST_CODE_TYPES.ID = COST_CODES.COST_CODE_TYPE_ID
=end
    field 'coiType', 'COIType'
=begin
    This field maps to the CODE_LOOKUPS table
    where CODE_LOOKUPS.ID = COST_CODES.COST_CODE_DIRECTION_ID
=end
    field 'coiDirection', 'COIDirection'
=begin
    This field maps to the COST_CODE_GRANULARITY_LEVELS table
    where COST_CODE_GRANULARITY_LEVELS.ID = COST_CODES.GRANULARITY_LEVEL_ID
=end
    field 'coiGranularities', :list, :element_type => 'COIGranularity'
=begin
    This field maps to the flag 'AUTOCOMPLETE' via the COST_CODE_FLAGS table
    where COST_CODES.ID = COST_CODE_FLAGS.COCD_ID
=end
    field 'generated', :boolean
  }

  # Facility Type - object that represents a Facility Type.
  # Extends Enumerated item maps to the CODE_LOOKUPS table
  # The shortCode field might be used as a unique identifier.
  define('FacilityType', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   :: LOGL
  # CODE_SETS.SET_DESC     :: 'Logistics Location Function Type'
  }

=begin
  Transport Type - object that represents a Transport Type
  Extends Enumerated Item
=end
  define('TransportType', :extends => 'EnumeratedItem') {
  }

=begin
  Order Request Type - object that represents a Order Request Type
  Extends Enumerated Item
=end
  define('OrderRequestType', :extends => 'EnumeratedItem') {
  }

=begin
  Order Valid Time - object that represents a Order Valid Time
  Extends Enumerated Item
=end
  define('OrderValidTime', :extends => 'EnumeratedItem') {
  }

=begin
  Order Basis - object that represents a Order Basis
  Extends Enumerated Item
=end
  define('OrderBasis', :extends => 'EnumeratedItem') {
  }

=begin
  Order Option Type - object that represents a Order Option Type
  Extends Enumerated Item
=end
  define('OrderOptionType', :extends => 'EnumeratedItem') {
  }

=begin
  Order Option Contract Type - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('OrderOptionContractType', :extends => 'EnumeratedItem') {
  }
=begin
  Vessel - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('Vessel', :extends => 'EnumeratedItem') {
  }

=begin
  DerivativeRight - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('DerivativeRight', :extends => 'EnumeratedItem') {
  }

=begin
  ExerciseType - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('ExerciseType', :extends => 'EnumeratedItem') {
  }

=begin
  FXRule - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('FXRule', :extends => 'EnumeratedItem') {
  }

=begin
  InterestRateType - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('InterestRateType', :extends => 'EnumeratedItem') {
  }

=begin
  FundamentalUOM - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('FundamentalUOM', :extends => 'EnumeratedItem') {
  }

=begin
  QPType - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('QPType', :extends => 'EnumeratedItem') {
  }

=begin
  PaymentTerm - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('PaymentTerm', :extends => 'EnumeratedItem') {
  }

=begin
  DocumentType - object that represents a Order Option Contract Type
  Extends Enumerated Item
=end
  define('DocumentType', :extends => 'EnumeratedItem') {
  }
}
