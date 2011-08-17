in_namespace('inner.security.currencies') {

  define('RdMinorUnit', :extends => 'RdLookup') {
  }

  # Maps to CURRENCIES  table
  define('RdCurrency', :extends => 'RdEntity') {
    constant 'descriptionFieldName', 'CURR_DESC'
    
    mixin('MainVersionable')
    field 'shortCode', :string
    field 'description', :string
    field 'synthetic', :boolean
    # Maps via CURRENCIES.STATUS_ID  field to STATUSES table
    field 'statusId', :string
    field 'numericCode', :string
    # Maps via CURRENCIES.MINOR_UNIT_ID field to CODE_LOOKUPS table
    field 'minorUnitId', :string
    field 'eligibleForSecurisation', :boolean
    field 'translations', :list, :element_type => 'RdTranslation'
    field 'holidayCalendarId', :string
  }

}