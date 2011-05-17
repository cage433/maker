package starling

import calendar.{HolidayTablesFactory, BusinessCalendars, NullHolidays}
import daterange.{Timestamp, DateRange, Day}
import eai.Book
import market.{FuturesExpiryRuleFactory, FuturesExpiryRules, FuturesExpiryRule}
import pivot._
import pivot.controller.PivotTableConverter
import pivot.model.{PivotTableModel, CollapsedState}
import utils.Log
import tradestore.TradePredicate

object TestLazy extends Application{

  val in = starling.services.StarlingInit.devInstance
  val ts = in.eaiTradeStores(Book.LondonDerivativesOptions)

  val pfs = PivotFieldsState(rowFields=List(Field("Strategy")), columnFields=List(Field("Instrument"), Field("Market")), dataFields=List(Field("Trade Count")))

  Log.infoWithTime("Init") { ts.init() }

  val pds = Log.infoWithTime("PDS") { ts.pivot(new Timestamp(), None, Day.today, TradePredicate.Null, Nil, true) }
  val pivotTable = Log.infoWithTime("pivotTable") { PivotTableModel.createPivotTableData(pds, pfs) }
  val grid = Log.infoWithTime("PTC") { new PivotTableConverter(OtherLayoutInfo(), pivotTable).createGrid() }

  println("Cols: " + grid.colData.size)
}