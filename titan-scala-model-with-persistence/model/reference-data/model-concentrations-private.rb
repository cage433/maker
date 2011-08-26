# this is the model for Reference Data Project
in_namespace('inner.referencedata.concentrations') {

  # Concentration Level Type - object that represents Concentration Level Type. Extends Lookup
  define('RdConcentrationLevelType', :extends => 'RdLookup') {
    constant 'Type Short Code', "CLTY"
    constant 'Maximum Country Limit', "CNTRY"
    constant 'Maximum Group Limit', "GRP"
    constant 'Maximum Obligor Limit', "OBLGR"
  }

  # Concentration Level - object that represents Concentration Level. Maps to CONCENTRATION_LEVEL table
  define('RdConcentrationLevel', :extends => 'RdEntity') {
    mixin('MainVersionable')
    field 'level', :integer
    field 'levelTypeId', :string
    field 'individualLevel', :real
    field 'groupLevel', :real
    field 'description', :string
    field 'statusId', :string
  }

  # Maps to CONCENTRATION_LIMITS  table
  define('RdConcentrationLimit', :extends => 'RdEntity') {
    mixin('MainVersionable')
    field 'recId', :string
    # Implied FK to CONCENTRATION_LEVELS (no VER)
    field 'derivedConcId', :string
    field 'derivedConcTypeId', :string
    field 'lastDerivedDatetime', :datetime
    field 'concLimitOverride', :boolean
    # Implied FK to CONCENTRATION_LEVELS (no VER)
    field 'overrideConcId', :string
    field 'lastOverrideDatetime', :datetime
    field 'lastOverrideById', :string
  }

}