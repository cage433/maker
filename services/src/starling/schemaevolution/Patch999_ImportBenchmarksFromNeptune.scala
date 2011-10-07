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


class Patch999_ImportBenchmarksFromNeptune extends Patch {
  override def deferredReason(context: PatchContext) =
    context.props.ImportBenchmarksFromNeptune() ? none[String] | some("Awaiting cutover from Neptune to Starling")

  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = init.marketDataStore.save(Map(
    MarketDataSet.ManualMetals → read(init.neptuneRichDB, Day.today).allValues))

  private def read(neptuneDB: RichDB, day: Day) =
    new NeptuneGradeAreaBenchmarkUtil(neptuneDB).read(day) ++
    new NeptuneCountryBenchmarksUtil(neptuneDB).read(day)
}

