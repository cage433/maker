package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import system.Patch
import starling.marketdata._
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.daterange.{ObservationPoint, ObservationTimeOfDay, Day}
import starling.db.{MarketDataEntry, MarketDataSet, DBWriter}
import starling.market._

class Patch148_BenchmarkDataChina1 extends Patch {

  override def requiresRestart = true // because this patch modifies markets which may have been read already.

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    //not needed anymore are a later patch generates the data
  }
}
