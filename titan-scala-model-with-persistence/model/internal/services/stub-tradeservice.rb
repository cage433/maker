in_namespace('Internal.Stub.TradeService') {
  service('StubTradeService') {

    operation('SetTrade') {
      parameter 'trade',    :RefinedMetalTrade
    }

    operation('ResetTrade') {
      parameter 'tradeId',	:integer
    }
  }
}
