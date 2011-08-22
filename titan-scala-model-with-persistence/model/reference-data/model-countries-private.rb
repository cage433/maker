# this is the model for Reference Data Project
in_namespace('inner.ReferenceData') {


  define('RdCountry') {
    constant 'classShortCode', 'COU'
    constant 'descriptionFieldName', 'COU_DESC'

    field 'id', :string
    mixin('MainVersionable')

    field 'shortCode', :string
    field 'description', :string
    field 'taxRate', :real, :optional => true
    field 'revertRisk', :boolean
    field 'eligible', :boolean
    field 'rank', :integer, :optional => true

    field 'currencyId', :string
    field 'vatId', :string
    field 'concId', :string
    field 'concRef', 'inner.referencedata.concentrations.RdConcentrationLimit'
    field 'statusId', :string

    field 'translations', :list, :element_type => 'RdTranslation'
    field 'countryRatingAgencyRefs', :list, :element_type => 'RdCountryRatingAgentRef'

    field 'countryGroupingsIds', :list, :element_type => :string
    field 'countryOrgsIds', :list, :element_type => :string
    field 'businessLinesIds', :list, :element_type => :string
  }

  # maps to the COU_RATING_AGENT_REFERENCES
  define('RdCountryRatingAgentRef', :extends => 'RdEntity') {
    mixin('ChildVersionable')
    field 'agentId',        :string
    field 'reference',   :string
    field 'countryId',        :string
    field 'countryVer',        :integer
  }


}

