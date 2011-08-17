#// This is the Refined metal Cash Flow Archive service interface
in_namespace('CostsAndIncomes.Internal.CostsRepository') {
  define('TACTICAL_UOMID') {
    constant 'DIMENSIONLESS', -1
    constant 'USD_PER_MTS', 0
    constant 'USD', 1
    constant 'RMB_PER_MTS', 2
    constant 'RMB', 3
    constant 'MTS', 4
    field 'dummy',              :integer
  }

  define('CostableAmount') {
    field 'amount',		:real
    field 'uom',                :integer
  }

  define('Cost') {
    field 'oid',                :integer
    field 'parentCostOid',      :integer, :optional => true
    field 'readOnly',	        :boolean
    field 'costCodeOid',	:integer
    field 'linkRefOid',         :integer
    field 'amount',		:real
    field 'uom',                :integer
    field 'state',              :string
    field 'version',            :integer
    field 'createTimeStamp',    :datetime
    field 'createUserOid',      :integer 
    field 'updateTimeStamp',    :datetime
    field 'updateUserOid',      :integer
    field 'comment',      	:string
  }

  define('CostCode') {
    field 'oid',                :integer
    field 'generated',          :boolean
    field 'code',		:string
    field 'description',	:string
    field 'costType',		:string
    field 'direction',		:string
    field 'directionId',	:string
  }

  define('GranularityLevel') {
    field 'oid',                :integer
    field 'parentOid',          :integer
    field 'name',               :string
  }

  define('LinkRecord') {
    field 'oid',                :integer
    field 'displayId',          :string
    field 'granularity',        'GranularityLevel'
  }

  define('CompletionRecord') {
    field 'prefix',          :string
    field 'linkRecords',     list('LinkRecord')
  }
  
  define('AncestorsWithSiblings') {
     field 'tradeLinkRecord', :LinkRecord
     field 'quotaLinkRecord', :LinkRecord
     field 'assignmentLinkRecord', :LinkRecord
     field 'tradeSiblings', list(:LinkRecord)
     field 'quotaSiblings', list(:LinkRecord)
     field 'assignmentSiblings', list(:LinkRecord)
  }


  service('GranularityRepository') { 
    operation('GetRootGranularityLevels', :returns => list('GranularityLevel')) {
    }
 
    operation('GetGranularityLevel', :returns => 'GranularityLevel') {
      parameter 'granularityLevelOid', :integer	
    }
 
    operation('GetGranularityLevelByName', :returns => 'GranularityLevel') {
      parameter 'name', :string
    }
 
    operation('GetChildGranularityLevels', :returns => list('GranularityLevel')) {
      parameter 'granularityLevelOid', :integer	
    }

    operation('InSubtree', :returns => :boolean) {
      parameter 'granularityOidRoot', :integer	
      parameter 'granularityOidOther', :integer	
    }

  }

  define('StandardEnrichmentDataKeys') {
    constant 'GROUP_COMPANY_NAME', 'groupcompany'
    constant 'COUNTERPARTY', 'counterparty'
    constant 'PURCHASE_OR_SALE', 'pors'
    constant 'COMMODITY', 'commodity'
    constant 'GRADE', 'grade'
    constant 'SHAPE', 'shape'
    constant 'STOCK_TYPE', 'stocktype'
    constant 'MARKET', 'market'
    field 'dummy',              :integer
  }

  service('LinkRefRepository') {
    operation('GetLinkRecord', :returns => 'LinkRecord') {
      parameter 'linkRefOid', :integer	
    }

    operation('GetLinkRecordsByGranularity', :returns => list('LinkRecord')) {
      parameter 'granularityLevelOid', :integer	
    }

    operation('GetChildren', :returns => list('LinkRecord')) {
      parameter 'linkRefOid', :integer	
    }

    operation('GetParentByGranularity', :returns => :LinkRecord) {
      parameter 'linkRefOid', :integer	
      parameter 'granularityLevelOid', :integer	
    }

    operation('GetAncestors', :returns => list('LinkRecord')) {
      parameter 'linkRefOid', :integer	
      parameter 'recurse', :boolean	
    }

    operation('GetAncestorsWithSiblings', :returns => :AncestorsWithSiblings) {
      parameter 'linkRefOid', :integer	
    }

    operation('GetAmount', :returns => 'CostableAmount') {
      parameter 'linkRefOid', :integer	
    }

    operation('GetEnrichmentData', :returns => :string) {
      parameter 'linkRefOid', :integer	
      parameter 'what', :string	
    }
  
    operation('GetIdCompletions', :returns => list('CompletionRecord')) {
      parameter 'prefix', :string	
    }
  }

  #// Common base class for concrete filter data returned from the server and ultimately recycled back for queries
  define('FilterBase') {
    field 'filterType', :enum, :enumerated => ['CostCodeSelect', 'FixedVariableSelect', 'CostCodeTypeSelect', 'DeliveryDate',
                                               'WarehouseLocationSelect', 'GroupCompanySelect', 'CommoditySelect', 'CounterpartySelect', 'HubSelect', 'OperatorSelect', 'AssignmentIdSearch', 'TradeIdSearch', 'QuotaIdSearch', 'MultipleLinkRefOidSelect']
  }

  #// returned by GetFilterListChoices when filter has a finite list of choices (probably from refdata)
  define('OidSelectFilter', :extends => 'FilterBase') {
    field 'itemOid', :integer
    field 'shortName', :string
    field 'longName', :string
  } 

  define('MultipleOidSelectFilter', :extends => 'FilterBase') {
    field 'itemOids', list(:integer)
  } 

  #// returned by GetFilterListChoices when filter has a finite list of choices (probably from refdata)
  define('GUIDSelectFilter', :extends => 'FilterBase') {
    field 'itemId', :string
    field 'shortName', :string
    field 'longName', :string
  } 

  #// For filters where we expect abitrary text input to do a like comparison. GetFilterListChoices should return an item of this type, with an empty string text field
  define('SearchFilter', :extends => 'FilterBase') {
     field 'text', :string
  }

  #// For filters where we expect abitrary text input to match precisely. GetFilterListChoices should return an item of this type, with an empty string text field
  define('TextMatchFilter', :extends => 'FilterBase') {
     field 'text', :string
  }

  #// For queries on dates (single day, before, after, etc) set fields as appropriate. GetFilterListChoices returns teh valid domain of the dates e.g. from ancient past to today, ot one week period, etc
  define('DateRangeFilter', :extends => 'FilterBase') {
     field 'startDateInclusive', :date
     field 'endDateInclusive', :date
  }

  define('ResultsBase',:abstract => true) {
    field 'status', :enum, :enumerated => ['OK', 'ValidationFailed', 'DataSyncError']
    field 'statusMessage', :string
  }

  define('BulkEditRow',:extends => 'ResultsBase') {
     field 'tradeLinkRecord', :LinkRecord
     field 'quotaLinkRecord', :LinkRecord
     field 'assignmentLinkRecord', :LinkRecord
     field 'amount', 'CostableAmount'
     field 'cost', :Cost
  }

  define('BulkEditorResults',:extends => 'ResultsBase') {
    field 'rows', list(:BulkEditRow)
  }

  define('BulkUpdateInstruction') {
    field 'modify', :boolean
    field 'cost', :Cost
  }

  service('CostsRepository') { 
    operation('GetCostCodes', :returns => list('CostCode')) { 
    }

    operation('GetCostCode', :returns => 'CostCode') { 
      parameter 'costCodeOid', :integer	
    }

    operation('AddCost', :returns => :integer) { 
      parameter 'cost', :Cost
    }

    operation('UpdateCost') { 
      parameter 'cost', :Cost
    }

    operation('DeleteCost') { 
      parameter 'cost', :Cost	
    }

    operation('GetCost', :returns => :Cost) { 
      parameter 'costOid', :integer	
    }

    operation('GetCosts', :returns => list('BulkEditRow')) {
      parameter 'linkRefOid', :integer	
    }

    operation('GetRolledUpCosts', :returns => list('BulkEditRow')) {
      parameter 'linkRefOid', :integer	
    }

    operation('GetValidCostCodes', :returns => list(:CostCode)) {
      parameter 'linkRefOid', :integer	
    } 

    operation('GetValidCostCodesForLevel', :returns => list(:CostCode)) {
      parameter 'granularityLevelOid', :integer	
    } 

    #// Use this to get concrete instances of EnumeratedFilter,FreeTextFilter,DateRangeFilter to populate the GUI elements
    operation('GetFilterListChoices', :returns => list(:FilterBase)) {
       parameter 'filterType', :FilterBase
       parameter 'granularityLevelOid', :integer	
    }

    #// Bulk Query for filtered list of any matching costs and their parent LinkRecords
    operation('GetBulkEditorRecords', :returns => 'BulkEditorResults') {
       parameter 'filters', list(:FilterBase)
       parameter 'granularityLevelOid', :integer	
    }

    #// Bulk Query for filtered list of any matching LinkRecords and if available any costs that also match the filter
    operation('GetBulkEditorAddRecords', :returns => 'BulkEditorResults') {
       parameter 'filters', list(:FilterBase)
       parameter 'granularityLevelOid', :integer	
    }

    operation('BulkEditorUpdateRecords', :returns => 'BulkEditorResults') {
       parameter 'filters', list(:FilterBase)
       parameter 'granularityLevelOid', :integer	
       parameter 'updates', list(:BulkUpdateInstruction)
    }

    operation('BulkEditorAddRecords', :returns => 'BulkEditorResults') {
       parameter 'filters', list(:FilterBase)
       parameter 'granularityLevelOid', :integer	
       parameter 'updates', list(:BulkUpdateInstruction)
    }

    operation('BulkEditorDeleteRecords', :returns => 'BulkEditorResults') {
       parameter 'filters', list(:FilterBase)
       parameter 'granularityLevelOid', :integer	
       parameter 'updates', list(:BulkUpdateInstruction)
    }
  }
}
