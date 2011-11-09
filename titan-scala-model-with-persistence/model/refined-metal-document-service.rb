#// This is the Refined metal document service interface
in_namespace('TradeCapture.Internal.RefinedMetalDocumentService') {

  define('BinaryData') {
    field 'data', :string
    field 'mimeType', :string
  }

  define('ReviewCase') {
    field 'binaryData', 'BinaryData'
    field 'tradeId', :integer
    field 'languageCode', :string 
    field 'reviewCaseType', :string   
  }

  # document definition response object, returns path and boolean indicating if the file exists
  define('DocumentDef') {
     field 'path',          :string    
     field 'exists',        :boolean 
  }

  define('ReviewCaseDef', :extends => 'DocumentDef') {
     field 'readOnly',      :boolean 
  }

  define('TranslationResponse') {
    field 'translationResult', :boolean
    field 'reqTranslations', list('RichTranslation')
  }

  define('DocumentResponse') {
    field 'oid',            :integer, :identifier => true
    field 'data',           'BinaryData'
    field 'translations',   'TranslationResponse'
  }

  # document retrieval / generation response, contains doc details or translations in the case of missing translations
  define('DocumentGenResp') {
    field 'transResp',      'TranslationResponse', :optional => true
    field 'docDefResp',     'DocumentDef', :optional => true
    field 'status',         :boolean
    field 'data',           'BinaryData'
  }
  
  service('GenerateDocuments') {

    # generate contract in translated language, may return a list of required translations
    operation('GenerateContract', :returns => 'TranslationResponse') {
      parameter 'id', :integer
      parameter 'language', 'Language'
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('GenerateDraft', :returns => 'TranslationResponse') {
      parameter 'id', :integer
      parameter 'language', 'Language'
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('GenerateSummary') {
      parameter 'id', :integer
    }

    operation('SaveReviewCase') {
       parameter 'reviewCase', 'ReviewCase'
    }

    operation('SubmitReviewCase') {
       parameter 'reviewCase', 'ReviewCase'
    }
    
    operation('DeleteDocuments', :returns => :boolean) {
      parameter 'id', :integer    
    }
  }

  service('RetrieveDocuments') {
  
    operation('RetrieveDocument', :returns => 'BinaryData') {
      parameter 'id', :string
      parameter 'documentType', :string
    }

    # get document in translated language
    operation('RetrieveDocumentForLanguage', :returns => 'DocumentResponse') {
      parameter 'id', :string
      parameter 'documentType', :string
      parameter 'language', 'Language'
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('SummaryDocument', :returns => :DocumentResponse) {
      parameter 'id', :integer
    }

    operation('DraftDocument', :returns => :DocumentDef) {
      parameter 'id', :integer
      parameter 'language', 'Language'
    }

    # get draft document, generate if it doesn't exist (if possible)
    operation('GetGenerateDraftDoc', :returns => :DocumentGenResp) {
      parameter 'id', :integer
      parameter 'language', 'Language'
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('ContractDocument', :returns => :DocumentDef) {
      parameter 'id', :integer
      parameter 'language', 'Language'
    }

    # get contract document, generate if it doesn't exist (if possible)
    operation('GetGenerateContractDoc', :returns => :DocumentGenResp) {
      parameter 'id', :integer
      parameter 'language', 'Language'
      parameter 'allowDefaultedTranslations', :boolean
    }
  }
}
