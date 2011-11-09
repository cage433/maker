#// This is the Refined metal trade service interface

in_namespace('TradeMgmt.Internal.RefinedMetalTrade') {
  service('CaptureTrades') {

    operation('Update', :returns => :RefinedMetalTradeSubmissionResult) {
      parameter 'trade', :RefinedMetalTrade
    }

    operation('Create', :returns => :RefinedMetalTradeSubmissionResult) {
      parameter 'trade', :RefinedMetalTrade
    }

    operation('Cancel', :returns => :RefinedMetalTradeSubmissionResult) {
      parameter 'trade', :RefinedMetalTrade
    }

    operation('Complete', :returns => :RefinedMetalTradeSubmissionResult) {
      parameter 'trade', :RefinedMetalTrade
    }

    operation('CalculatePremia', :returns => :RefinedMetalTrade) {
      parameter 'trade', :RefinedMetalTrade
    }
  } 

  define('NeptuneQuotaInfo') {
		field 'quotaId',	:integer
		field 'quotaName',	:string
		field 'quantity',	:integer
  }

  define('NeptuneResult') {
    field 'success', :boolean
		field 'quotaInfo',	:list, :element_type => 'NeptuneQuotaInfo'
    field 'neptuneId', :string
		field 'error',	:string
  }

  define('NeptuneLoginResult') {
    field 'success', :boolean
    field 'username', :string
  }
  
  
  define('SearchResult') {
    field 'exist', :boolean
    field 'trade', :RefinedMetalTrade, :optional => true
  }
  
  

  # this is the Neptune trade capture interface
  service('CaptureNeptuneTrades') {
    operation('Create', :returns => 'NeptuneResult') {
      parameter 'trade', :RefinedMetalTrade
    }
    operation('Destroy', :returns => 'NeptuneResult') {
      parameter 'neptuneId', :string
      parameter 'groupCompanyId', :integer
    }
    operation('Complete', :returns => 'NeptuneResult') {
      parameter 'neptuneId', :string
    }
    operation('ApplyPriceFixations', :returns => 'NeptuneResult') {
      parameter 'trade', :RefinedMetalTrade
    }

    # and the Neptune administrative interface
    operation('Login', :returns => 'NeptuneLoginResult') {
    }
    operation('GetVersion', :returns => :string) {
    }
    operation('GetBuildConfiguration', :returns => :string) {
    }
    operation('GetProcessId', :returns => :integer) {
    }
    operation('GetNeptuneQuotaInfo', :returns => 'NeptuneResult') {
      parameter 'neptuneId', :string
      parameter 'groupCompanyId', :integer          
    }
  }

  service('GetTrades') {
    operation('GetAll', :returns => list('RefinedMetalTrade')) {
    }
    operation('Get', :returns => :RefinedMetalTrade) {
      parameter 'oid', :integer	
    }
    operation('GetByNeptuneId', :returns => :RefinedMetalTrade) {
      parameter 'neptuneId', :string	
    }
    operation('GetBySearchPattern', :returns => :SearchResult) {
      parameter 'neptuneId', :string
    }
  }

  define('SyncResult') {
    field 'success', :boolean
    field 'message', :string
    field 'tradeId', :string
    field 'resultInfo',  :enum, :enumerated => ['ErrorReadingNeptune', 'ErrorUpdatingTitan', 'UpdatedTitan', 'NoDiffWithTitan', 'CreatedInTitan', 'ErrorCreatingInTitan']
  }

  service('SyncTrades') {
    operation('SyncCompletedTradeWithNeptune', :returns => 'SyncResult') {
      parameter 'neptuneId', :string
    }
  }

  define('FilteredTradeBlotterRowResults') {
    field 'filtered',        :boolean
    field 'tradeBlotterRow', :TradeBlotterRow
  }

  service('BrowseTrades') {
    operation('GetTradeBlotterRowById', :returns => :TradeBlotterRow) {
      parameter 'oid', :integer
    }
    operation('GetFilteredTradeBlotterRowById', :returns => :FilteredTradeBlotterRowResults) {
      parameter 'filterByWho', :integer
      parameter 'filterByDate', :integer
      parameter 'oid', :integer
    }
    operation('GetNotCancelledTradeBlotterRows', :returns => list('TradeBlotterRow')) {
    }
    operation('GetNotCancelledTradeBlotterRowsFromId', :returns => list('TradeBlotterRow')) {
      parameter 'id', :integer
      parameter 'count', :integer
    }
    operation('GetFilteredNotCancelledTradeBlotterRowsFromId', :returns => list('TradeBlotterRow')) {
      parameter 'filterByWho', :integer
      parameter 'filterByDate', :integer
      parameter 'id', :integer
      parameter 'count', :integer
    }
    operation('GetTradeBlotterQuotaRowsForTrade', :returns => list('TradeBlotterQuotaRow')) {
      parameter 'oid', :integer
      parameter 'sortBy', :string
    }
    operation('CheckTradeWithIdExists', :returns => :boolean) {
      parameter 'oid', :integer
    }
    operation('CheckTradeWithNeptuneIdExists', :returns => :boolean) {
      parameter 'neptuneId', :string
    }
  }
}
