#// This is the EDM trade service interface definition

in_namespace('EDM.TradeMgmt.Trade') {

  define('TradeSubmissionResult') {
    field 'trade',                    'Trade'
    field 'errors',                   :list, :element_type => :string
  }

  # contains an EDM trade, OR an error message indicating a problem
  #// with that trade
  define('TradeResult') {
    field 'trade', :Trade
    field 'error', :string
  }

  # meta info for EDM trade results. 'cached' indicates that the
  #// cache is populated or not. If not then the service is loading the
  #// cache and a retry is advisable
  define('TradeResults') {
    field 'cached', :boolean
    field 'results', :list, :element_type => 'TradeResult'
  }

# this is the interface that returns real EDM (not translated) trades
  service('EdmGetRealTrades') {

    operation('GetAll', :returns => 'TradeResults') {
    }

    operation('GetByOid', :returns => :Trade) {
      parameter 'oid', :integer
    }
  }

# this is the interface that returns translated EDM (not real) trades
#// The trades are migrated from the refined metal store
  service('EdmGetTrades') {

    operation('GetAll', :returns => 'TradeResults') {
    }

    operation('GetByOid', :returns => :Trade) {
      parameter 'oid', :integer
    }
  }

  service('EdmCaptureTrades') {
    operation('Create', :returns => :TradeSubmissionResult) {
      parameter 'trade', :Trade
    }
    operation('Update', :returns => :TradeSubmissionResult) {
      parameter 'trade', :Trade
    }
  }
}
