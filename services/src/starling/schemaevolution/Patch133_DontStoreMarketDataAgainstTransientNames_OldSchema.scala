package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.Patch
import starling.dbx.QueryBuilder._


class Patch133_DontStoreMarketDataAgainstTransientNames_NewSchema extends Patch {
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