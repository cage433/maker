package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchContext, Patch}
import starling.dbx.QueryBuilder._


class Patch128_DontStoreMarketDataAgainstTransientNames_OldSchema extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.delete("MarketData", "marketDataType" like "%FreightParityDataType%")
    writer.delete("MarketData", "marketDataType" like "%CountryBenchmarkDataType%")
    writer.delete("MarketData", "marketDataType" like "%GradeAreaBenchmarkDataType%")
  }
}

class Patch128_DontStoreMarketDataAgainstTransientNames_NewSchema extends Patch {
  override def deferredReason(context: PatchContext) = context.dependsOn[Patch120_MigrateMarketDataToFasterSchema]

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
      DELETE FROM MarketDataValue
      WHERE extendedKey in (
        SELECT id from MarketDataExtendedKey WHERE marketDataType in ('FreightParity', 'CountryBenchmark', 'GradeAreaBenchmark')
      )
    """)

    writer.delete("MarketDataExtendedKey", "marketDataType" in List("FreightParity", "CountryBenchmark", "GradeAreaBenchmark"))
    writer.delete("MarketDataValueKey", "valueKey" like "%NeptuneCountry>%")
  }
}