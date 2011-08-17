expose 'Event'
expose 'EventKey'

in_namespace('Shared.Events') {

  # this is the enterprise architecture approved event
 
  define('EventKey') {
      field 'id',              :string
  }

  define('Payload') {
      field 'payloadType',  :string
      field 'key',          'EventKey'
      field 'source',       :string
      field 'timestamp',    :datetime
  }

  define('Body') {
      field 'payloads',      :list, :element_type => 'Payload'
  }

  define('Header') {
      field 'messageHash',     :string
      field 'timestamp',       :datetime
      field 'host',            :string
      field 'pid',             :integer
  }

  define('Content') {
      field 'header',       'Header'
      field 'body',         'Body'
  }


  define('Event') {
    constant 'TradeSubject', 'trade'
    constant 'CostsAndIncomesSubject', 'CostsAndIncomes'
    constant 'CostsAndIncomesIdPayload', 'costId'
    constant 'DocSubject', 'document'
    constant 'NeptuneTradeSubject', 'neptune trade'
    constant 'MessageSubject', 'message'
    constant 'ReferenceDataSubject', 'ReferenceData'
    constant 'InventorySubject', 'inventory'
    constant 'AllocationSubject', 'inventory allocation'
    constant 'LogisticsQuotaSubject', 'logistics quota'
    constant 'MovementSubject', 'movement'
    constant 'MovementAssignmentSubject', 'movement assignment'
    constant 'MovementWeightChangeSubject', 'movement weight change' 
    constant 'logisticsTaskSubject', 'logistics task'
    constant 'starlingValuationServiceSubject', 'valuation service'

    #EDM Subjects
    constant 'EDMLogisticsInventorySubject', 'EDMLogisticsInventory'
    constant 'EDMLogisticsSalesAssignmentSubject', 'EDMLogisticsSalesAssignment'

    constant 'SummaryPayload', 'summary'
    constant 'ContractPayload', 'contract'
    constant 'DraftPayload', 'draft'
    constant 'MessagePayload', 'message'
    constant 'FailedPayload', 'failed'
    constant 'RefinedMetalTradeIdPayload', 'Refined Metal Trade'
    constant 'TradeStatusPayload', 'Trade Status'
    constant 'DocumentLocationPayload', 'Document Location'
    constant 'ReferenceDataChangedPayload', 'referenceData'
    constant 'ReferenceDataItemChangedPayload', 'referenceDataItem'
    constant 'InventoryIdPayload', 'Inventory'
    constant 'LogisticsQuotaIdPayload', 'LogisticsQuota'
    constant 'MovementIdPayload', 'Movement'
    constant 'LogisticsTaskGuidPayload', 'LogisticsTask'
    constant 'VersionPayload', 'Version'
    constant 'MovementVersionPayload', 'MovementVersion'
    constant 'InventoryVersionPayload', 'InventoryVersion'

    #EDM Payloads
    constant 'EDMLogisticsInventoryIdPayload', 'EDMLogisticsInventoryId'
    constant 'EDMLogisticsAssignmentIdPayload', 'EDMLogisticsAssignmentId'

    constant 'ScratchTradeIdentifier', 'scratch'
    constant 'CompletingTradeIdentifier', "completing"
    constant 'CompletedTradeIdentifier', 'completed'

    # these will likely be Spring configured on the Scala side
    constant 'MurdochSource',       'murdoch'
    constant 'TrademgmtSource',     'trademgmt'
    constant 'ReferencedataSource', 'referencedata'
    constant 'StarlingSource',      'starling'

    # Document generation language
    constant 'DocumentLanguageEnglish', 'english'
    constant 'DocumentLanguageChinese', 'chinese'

    constant 'LanguagesForWhichTranslationsChanged', 'Translations'
    constant 'TranslationsIdentifier', 'Translations'
    constant 'TranslationsPayload', 'Translations'
	
    # Field definitions
    field 'verb',            :enum, :enumerated =>['New', 'Cancel', 'Updated', 'Removed', 'Replaced']
    field 'subject',         :string
    field 'key',             'EventKey'
    field 'source',          :string
    field 'content',         'Content'
  }
}
