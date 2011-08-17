in_namespace('inner.security.restrictions') {

    # Entity Type
  define('RdPropertyEntity', :extends => 'RdLookup') {
    constant 'Entity', "ENTTY"
    constant 'User', "USR"
  }

  # Property
  define('RdPropertyEntityVariable', :extends => 'RdLookup') {
    constant 'Entity Country', "EC"
    constant 'Entity Region', "ER"
    constant 'User Country Of Residence', "UCR"
    constant 'User Group Company Company Of Incorporation', "UGPC"
  }

  define('RdRestrictionCategory', :extends => 'RdLookup') {
    constant 'Sanction', "RSTC"
  }

  # Maps to PROPERTIES table
  define('RdProperty', :extends => 'RdEntity') {
    field 'name', :string
    field 'description', :string
    field 'entityId', :string
    field 'entityVariableId', :string
    field 'vealueCodesetId', :string
    field 'entityVariableValueIsLookup', :boolean
    field 'modifiedBy', :string
  }

  # Maps to RESTRICTION_PROPERTIES table
  define('RdRestrictionProperty', :extends => 'RdEntity') {
    field 'property', 'RdProperty'
    field 'restrictionId', :string
    field 'lookupValueId', :string
    field 'specificValue', :string
    field 'modifiedBy', :string
  }

  # Maps to RESTRICTIONS table
  define('RdRestriction', :extends => 'RdEntity') {
    field 'name', :string
    field 'description', :string
    field 'statusId', :string
    field 'categoryId', :string
    field 'modifiedBy', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime, :optional => true
    field 'restrictionProperties', :list, :element_type => 'RdRestrictionProperty'
  }

  define('HProperty', :extends => 'RdProperty') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRestrictionProperty', :extends => 'RdRestrictionProperty') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRestriction', :extends => 'RdRestriction') {
    field 'verId', :string
    field 'verStart', :datetime
  }

}