package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import system.Patch
import starling.marketdata._
import starling.quantity.UOM._
import starling.daterange.{ObservationPoint, Day}
import starling.db.{MarketDataEntry, MarketDataSet, DBWriter}
import starling.market._
import starling.quantity.{UOM, Quantity}

class Patch150_VatRateNoDay extends Patch {

  override def requiresRestart = true // because this patch modifies markets which may have been read already.

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val patchUtil = MarketDataPatchUtil(starling, writer)
    patchUtil.deleteMarketData(true, MarketDataTypeName("FreightParity"), MarketDataTypeName("GradeAreaBenchmark")) //not used
    patchUtil.deleteMarketData(true, MarketDataTypeName("ShanghaiVAT"))
    val entries = List(
      MarketDataEntry(ObservationPoint.RealTime, ShanghaiVATDataKey(), ShanghaiVATData(new Quantity(0.17, UOM.PERCENT)))
    )
    starlingInit.marketDataStore.save(Map(MarketDataSet.ManualMetals  → entries))
  }
}
