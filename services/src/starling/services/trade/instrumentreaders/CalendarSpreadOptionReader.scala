package starling.services.trade.instrumentreaders

import starling.instrument.CalendarSpreadOption
import starling.services.trade.ExcelRow

class CalendarSpreadOptionReader extends ExcelInstrumentReader {

  def canRead(row: ExcelRow) = row.instrumentType == CalendarSpreadOption

  def create(row: ExcelRow) = {
    val market = row.futuresMarket
    val volume = row.volume
    val strike = row.strike
    val cp = row.callOrPut
    val period = row.period
    assert(row.exerciseType.isEmpty, "CSO can't have a specified exercise type.")
    
    CalendarSpreadOption(market, period, strike, volume, cp)
  }
}