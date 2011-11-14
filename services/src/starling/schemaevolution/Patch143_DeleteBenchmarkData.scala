package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch143_DeleteBenchmarkData extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    MarketDataPatchUtil(writer).deleteMarketData(true, List("GradeAreaBenchmark", "CountryBenchmark"))
  }
}