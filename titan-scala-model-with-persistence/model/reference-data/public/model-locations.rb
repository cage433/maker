# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Locations
in_namespace('commondomains.referencedata.masterdata.locations') {
  # The following class is used only in this package to make possible
  # to search / filter for a specific location
  define('LogLocCriteria') {
    # Criteria to narrow the result by 'FacilityType'
    field 'facType',                  :string, :optional => true
    # Criteria to narrow the result by 'Country'
    field 'countryCode',              :string, :optional => true
  }

  define('PoliticalUnit', :extends => 'Entity', :abstract => true) {
    field 'entityRatings',            :list, :element_type => 'EntityRating'
  }

=begin
  UN Locations
  maps to LOCODES tableAccountType
=end
  define('UNLocation', :extends => 'PoliticalUnit') {
=begin
    This field might be used as unique identifier.
    This field maps to LOCODES.LOCATION_CODE
=end
    field 'shortCode',                :string
    # This field maps to LOCODES.LOCATION_NAME
    field 'description',              :string
=begin
    This field maps to LOCATION_FUNCTION_TYPES table
    where LOCATION_FUNCTIONS.LOCO_ID = LOCODES.ID
      and LOCATION_FUNCTIONS.FUNC_ID = LOCATION_FUNCTION_TYPES.ID
=end
    field 'locationFunctions',        :list, :element_type => 'LocationFunction'
    # This field maps to LOCODES.LATITUDE
    field 'latitude',                 :real
    # This field maps to LOCODES.LONGITUDE
    field 'longitude',                :real
  }

=begin
  TODO: non of the fields below appear on the EDM
  UN Sub Divisions which may containt UN Locations
  maps to the SUBDIVISION table
=end
  define('UNSubDivision', :extends => 'PoliticalUnit') {
=begin
    This field might be used as unique identifier.
    This field points to SUBDIVISION.SUB_CODE
=end
    field 'shortCode',                :string
    # This field points to SUBDIVISION.SUB_DESC
    field 'description',              :string
=begin
    This field maps to the LOCODES table
    where LOCODES.SUBDIVISION_ID = SUBDIVISION_ID
=end
    field 'unLocations',              :list, :element_type => 'UNLocation'
  }

=begin
  ConcentrationLevel - object that represents concentration level.
  maps to the CONCENTRATION_LEVEL table
=end
  define('ConcentrationLevel', :extends => 'Entity') {
    # CONCENTRATION_LEVEL.LEVEL_IDENTIFIER
    field 'level',            :integer
    field 'levelType',        'ConcentrationLevelType'
    field 'individualLevel',  :real
    field 'groupLevel',       :real
  }

=begin
  Country object which maps back to the COUNTRIES table
=end
  define('Country', :extends => 'PoliticalUnit') {
=begin
    This field might be used as unique identifier.
    This field points to COUNTRIES.ISO_CODE
=end
    field 'shortCode',                :string
    # This field points to COUNTRIES.COUNTRY_DESC
    field 'description',              :string

    field 'withholdingTaxRatePercentage', :real
=begin
    This field maps to the CODE_LOOKUPS table
    where CODE_LOOKUPS.ID = COUNTRIES.CONC_ID
=end
    field 'concentrationLevel',       'ConcentrationLevel'
=begin
    This field maps to the CODE_LOOKUPS table
    where CODE_LOOKUPS.ID = COUNTRIES.VAT_ID
=end
    field 'vatCode',                  'VatCode'
=begin
    This field maps to the CODE_LOOKUPS table
    where CODE_LOOKUPS.ID = COUNTRIES.CCY_ID
=end
    field 'currency',                 'commondomains.referencedata.masterdata.unitsofmeasure.Currency'

=begin
    This field maps to the LOCODES table
    where LOCODES.COU_ID = COUNTRIES.ID
=end
    field 'unLocations',              :list, :element_type => 'UNLocation'
=begin
    This field maps to the SUBDIVISION table
    where SUBDIVISION.COU_ID = COUNTRIES.ID
=end
    field 'unSubDivisions',           :list, :element_type => 'UNSubDivision'
=begin
    # Country Tax Rate
    field 'taxRate',                  :real, :optional => true
    # Eligible for securitisation
    field 'eligible',                 :boolean
    # Revert risk
    field 'revertRisk',               :boolean
=end
  }

=begin
  International Organisation - object that represents International Organisation.
  Extends enumerated item however it's abstract therefore doesn't maps to anything
  The shortCode field might be used as a unique identifier.
=end
  define('PoliticalUnitGrouping', :extends => 'EnumeratedItem', :abstract => true) {
  }

=begin
  International Organisation - object that represents International Organisation.
  Extends enumerated item and maps to CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('InternationalOrganization', :extends => 'PoliticalUnitGrouping') {
    # CODE_SETS.SHORT_CODE   ::  
    # CODE_SETS.SET_DESC     ::
  }

=begin
  Continent - object that represents continents.
  Extends enumerated item and maps to CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier.
=end
  define('Continent', :extends => 'PoliticalUnitGrouping') {
    # CODE_SETS.SHORT_CODE   ::  
    # CODE_SETS.SET_DESC     ::
  }

=begin
  Region - object that represents regions.
  Extends enumerated item and maps to CODE_LOOKUPS table
  The shortCode field might be used as a unique identifier
=end
  define('Region', :extends => 'PoliticalUnitGrouping') {
    # CODE_SETS.SHORT_CODE   ::  
    # CODE_SETS.SET_DESC     ::
  }

  # Logistic Location - Object that represents Logistics locations. It's an abstract superclass which can be subclassed
	# please be aware that this class doesn't maps to any table however the subclasses should
	# map to LOGISTICS_LOCATIONS
  define('LogisticsLocation', :extends => 'Entity') {
    #this field maps to LOGISTICS_LOCATIONS.SHORT_CODE
    field 'shortCode',                  :string
    # This field maps to LANGUAGE_VERSIONS.TRANS column
    # where LANGUAGE_VERSIONS.REC_ID = LOGISTICS_LOCATIONS.ID
    #  and LANGUAGE_VERSIONS.REC_VER = LOGISTICS_LOCATIONS.VER
    #  and FIELD_NAME = 'FAC_NAME'
    field 'description',              :string
    # This field maps to the LOCODES table via the LOCO_ID column
    # field 'location',                 'UNLocation', :optional => true
    # Needs to go into the model
    # field 'address',                  'Address', :optional => true
    # This field maps to the COUNTRIES.ISO_CODE field via the COU_ID column 
    field 'countryCode',              :string
    # This field maps to the CODE_LOOKUPS table via the LOGISTICS_FUNCTIONS
    # where LOGISTICS_FUNCTIONS.LOGLOC_ID = LOGISTICS_LOCATIONS.ID
    #   and LOGISTICS_FUNCTIONS.LOGLOC_VER = LOGISTICS_LOCATIONS.VER
    #   and LOGISTICS_FUNCTIONS.FAC_TYPE_ID = CODE_LOOKUPS.ID
    field 'facilityType',             :list, :element_type => 'FacilityType'
    # Maps via STATUS.ID = LOGISTICS_LOCATIONS.STATUS_ID
    field 'status',                   'Status'
    # VER_START + VER_END - A date range when the record is active. 
    # The end date can be null.
    field 'effectiveDateRange',       'commontypes.dataspecifications.DateRange'
  }
}
