in_namespace('inner.common.filtering') {

  define('KeyValuePair') {
    field 'key',    :string
    field 'value',  :string
  }

  define('KeyValuesPair') {
    field 'key',    :string
    field 'values', :list, :element_type => :string
  }

  define('ConditionalExpr', :abstract => true) {
  }

  define('AtomExpr', :extends => 'ConditionalExpr') {
    # op = 'EQUALS' or 'LIKE'
    field 'op',            :string

    # Entity property name or path
    field 'propertyName',  :string

    # A value for comparision
    field 'value',         :string  
  }

  define('BinaryOperation', :extends => 'ConditionalExpr') {
    # op = 'AND' or 'OR'
    field 'op',            :string

    # The left atgument of the 'op' operation
    field 'leftArg',       'ConditionalExpr'

    # The right atgument of the 'op' operation
    field 'rightArg',      'ConditionalExpr'        
  }

  define('RdEntityFilter') {
    field 'condExpr',      'ConditionalExpr'
    field 'sorts',         :list, :element_type => 'KeyValuePair'
    field 'startIndex',    :integer
    field 'count',         :integer
  }

}

in_namespace('inner.ReferenceData.Locations.dto') {

  define('UNLocationsFilter') {
      field 'conditions',          :list, :element_type => 'KeyValuesPair'
      field 'conditionsLinkingOp', :string
      field 'sorts',               :list, :element_type => 'KeyValuePair'
      field 'startIndex',          :integer
      field 'count',               :integer
  }
  
}

in_namespace('inner.ReferenceData.LegalEntities.dto') {

  define('GroupCompaniesFilter') {
      field 'conditions',          :list, :element_type => 'KeyValuesPair'
      field 'conditionsLinkingOp', :string
      field 'sorts',               :list, :element_type => 'KeyValuePair'
      field 'startIndex',          :integer
      field 'count',               :integer
      field 'groups',              :list, :element_type => 'KeyValuePair'
  }
  
}


