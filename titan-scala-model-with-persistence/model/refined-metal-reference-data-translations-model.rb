# this is the model as used for Release 1 of RMET

# Translation service model for Titan tactical ref-data
in_namespace('TradeCapture.Internal.RefinedMetal') {

  # these objects are to allow multiple languages for reference data
  define('Language') {
    field 'oid', :integer, :identifier => true
    field 'code',                   :string
    field 'name',                   :string
    field 'optional',               :boolean
  }

  define('RefDataBase') {
    field 'oid', :integer, :identifier => true
  }

  define('Translation') {
    constant 'translatedLength'     , 255
    field 'oid', :integer, :identifier => true
    field 'language',               :integer_key, :references => 'TradeCapture.Internal.RefinedMetal.Language(oid)'
    field 'refDataBase',            :integer_key, :references => 'RefDataBase(oid)'
    field 'field',                  :string
    field 'translated',             :string, :max_length => Translation::TRANSLATEDLENGTH
  }

  define('RichTranslation') {
    field 'reqTranslation',         'TradeCapture.Internal.RefinedMetal.Translation'
    field 'text',                   :string
    field 'typeName',               :string
  }

  define('Translations') {
    field 'oid', :integer, :identifier => true
    field 'translatedItems',        :list, :element_type => 'TradeCapture.Internal.RefinedMetal.Translation'
    field 'reqTranslations',        :list, :element_type => 'TradeCapture.Internal.RefinedMetal.Translation'
  }
}
