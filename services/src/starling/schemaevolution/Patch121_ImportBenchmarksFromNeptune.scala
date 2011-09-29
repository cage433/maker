package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.daterange.Day
import starling.curves.readers._
import scalaz.Scalaz._
import starling.db.{MarketDataSource, MarketDataSet, DBWriter}
import system.{PatchContext, Patch}


class Patch121_ImportBenchmarksFromNeptune extends Patch {
  override def deferredReason(context: PatchContext) =
    context.props.ImportBenchmarksFromNeptune() ? none[String] | some("Awaiting cutover from Neptune to Starling")

  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = init.marketDataStore.save(Map(
    MarketDataSet.ManualMetals → dataSource(init.neptuneRichDB).read(Day.today).values.toList.flatten))

  private def dataSource(neptuneDB: RichDB): MarketDataSource =
    new NeptuneGradeAreaBenchmarksMarketDataSource(neptuneDB) + new NeptuneCountryBenchmarksMarketDataSource(neptuneDB)
}

class Patch121_ImportFreightParityFromNeptune extends Patch {
  override def deferredReason(context: PatchContext) =
    context.props.ImportFreightParityFromNeptune() ? none[String] | some("Awaiting cutover from Neptune to Starling")

  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = init.marketDataStore.save(Map(
    MarketDataSet.ManualMetals → dataSource(init.neptuneRichDB).read(Day.today).values.toList.flatten))

  private def dataSource(neptuneDB: RichDB): MarketDataSource = new NeptuneFreightParityMarketDataSource(neptuneDB)
}