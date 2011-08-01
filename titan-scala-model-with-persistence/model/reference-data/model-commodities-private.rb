# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.Commodities') {
=begin

=end


  define('RdRefinedMetal', :extends => 'RdLookup') {
    constant 'classShortCode', 'RMET'

    field 'gradesIds', :list, :element_type => :string
    field 'shapesIds', :list, :element_type => :string
    field 'brandsIds', :list, :element_type => :string
  }


  define('RdRefinedMetalGrade', :extends => 'RdLookup') {
    constant 'classShortCode', 'RGRAD'
  }
  

  define('RdRefinedMetalShape') {
    constant 'classShortCode', 'CSH'
    constant 'descriptionFieldName', 'SHAP_DESC'

    field 'id', :string
    mixin('MainVersionable')

    field 'shortCode',               :string
    field 'description',             :string
    field 'specification',           :string

    field 'statusId', :string

    field 'translations',            :list, :element_type => 'RdTranslation'

    field 'businessLinesIds',        :list, :element_type => :string

  }


  define('RdRefinedMetalBrand') {
    constant 'classShortCode', 'CBRND'
    constant 'descriptionFieldName', 'BRND_DESC'

    field 'id', :string
    mixin('MainVersionable')

    field 'shortCode',               :string
    field 'description',             :string
    field 'lmeRegisterable',         :boolean

    field 'statusId', :string

    field 'translations',            :list, :element_type => 'RdTranslation'

    field 'businessLinesIds',        :list, :element_type => :string

  }
}