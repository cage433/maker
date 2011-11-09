in_namespace('EDM.TradeMgmt.MaterialSpecification') {

  define('MaterialSpec') {
    constant 'commentsLength', 255
    field 'oid',                                :integer, :identifier => true
    field 'comments',                           :string, :max_length => MaterialSpec::COMMENTSLENGTH
  }

  define('ProductSpec', :extends => 'MaterialSpec') {
    field 'dutyPaid',                           :boolean
  }

  define('CommoditySpec', :extends => 'ProductSpec') {
     field 'commodity',                         :guid
  }

  define('RefinedMetalSpec', :extends => 'CommoditySpec') {
     field 'shape',                             :guid
     field 'grade',                             :guid
#     field 'brands',                           :list, :element_type => 'RefinedMetalBrand'
  }
}
