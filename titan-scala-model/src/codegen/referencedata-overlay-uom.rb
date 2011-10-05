in_namespace('inner.ReferenceData.uom') {

  define('RdFundamentalUnit') {
    field 'translations',      :scala_list_type => :set, :scala_use_java_backing => true
  }

  define('RdCompoundUnit') {
    field 'items',             :scala_list_type => :set, :scala_use_java_backing => true
    field 'translations',      :scala_list_type => :set, :scala_use_java_backing => true
  }

}
