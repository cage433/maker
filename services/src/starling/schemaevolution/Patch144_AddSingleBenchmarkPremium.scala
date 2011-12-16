package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.daterange.Day
import starling.db.MarketDataEntry
import starling.daterange.ObservationPoint
import starling.daterange.{ObservationTimeOfDay, Day}
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.db.{MarketDataSet, DBWriter}
import starling.market.Copper
import starling.marketdata.{GradeCode, NeptuneCountryCode, CountryBenchmarkData, CountryBenchmarkMarketDataKey}

/**
 * Market data editor can't work at present if there is no data for a given type. A fix
 * is on its way but for now we're writing a row with a very large premium,
 * just so it's obviously wrong in the unlikely event we have hit on a country/grade combination that is used.
 */
class Patch144_AddSingleBenchmarkPremium extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    //not needed anymore are a later patch generates the data
  }
}
