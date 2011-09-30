package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.daterange.Day
import starling.curves.readers._
import system.{PatchContext, Patch}
import collection.immutable.Map
import starling.db.{MarketDataSet, DBWriter}
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._


class Patch121_ImportBenchmarksFromNeptune extends Patch {
  override def deferredReason(context: PatchContext) =
    context.props.ImportBenchmarksFromNeptune() ? none[(Int, String)] | some((121, "Awaiting cutover from Neptune to Starling"))

  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = init.marketDataStore.save(Map(
    MarketDataSet.ManualMetals → read(init.neptuneRichDB, Day.today).allValues))

  private def read(neptuneDB: RichDB, day: Day) =
    new NeptuneGradeAreaBenchmarksMarketDataSource(neptuneDB).read(day) ++
    new NeptuneCountryBenchmarksMarketDataSource(neptuneDB).read(day)
}

class Patch121_ImportFreightParityFromNeptune extends Patch {
  override def deferredReason(context: PatchContext) =
    context.props.ImportFreightParityFromNeptune() ? none[(Int, String)] | some((121, "Awaiting cutover from Neptune to Starling"))

  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = init.marketDataStore.save(Map(
    MarketDataSet.ManualMetals → read(init.neptuneRichDB, Day.today).allValues))

  private def read(neptuneDB: RichDB, day: Day) = new NeptuneFreightParityMarketDataSource(neptuneDB).read(day)
}