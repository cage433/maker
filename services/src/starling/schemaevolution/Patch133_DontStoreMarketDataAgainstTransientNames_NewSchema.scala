package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.Patch


class Patch133_DontStoreMarketDataAgainstTransientNames_NewSchema extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) =
    MarketDataPatchUtil(starling, writer).deleteMarketData(typeNames = List("FreightParity", "CountryBenchmark", "GradeAreaBenchmark"))
}