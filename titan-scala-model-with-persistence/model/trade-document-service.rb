in_namespace('TradeCapture.Internal.TradeDocumentService') {
  service('TradeDocumentService') {

    # get doc params in the required lang / generate a list of missing translations
    operation('GetContractDocumentParameters', :returns => :CDocParsResp) {
      parameter 'tradeid', :integer
      parameter 'langCode', :string
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('GetDraftDocumentParameters', :returns => :CDocParsResp) {
      parameter 'tradeid', :integer
      parameter 'langCode', :string
      parameter 'allowDefaultedTranslations', :boolean
    }

    operation('RegetDraftDocumentParameters', :returns => :ContractDocumentParameters) {
      parameter 'tradeid', :integer
    }

    operation('GetSummaryDocumentParameters', :returns => :SummaryDocumentParameters) {
      parameter 'tradeid', :integer
    }

    operation('GetDocumentStorageParameters', :returns => :DocumentStorageParameters) {
      parameter 'tradeid', :integer
    }

    operation('GetGroupCompanyIdFromTradeId', :returns => :integer) {
      parameter 'oid', :integer
    }
  }
}