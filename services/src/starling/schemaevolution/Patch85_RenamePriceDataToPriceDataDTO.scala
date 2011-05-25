package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.utils.StarlingXStream
import starling.marketdata.{PriceDataDTO, PriceData}
import starling.utils.ImplicitConversions._


class Patch85_RenamePriceDataToPriceDataDTO extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val renamer = new XStreamRenamer(classOf[PriceData].getName → classOf[PriceDataDTO])

    starling.inTransaction { writer =>
      writer.queryForUpdate("select data from MarketData where data is not null and marketDataType = '<starling.marketdata.PriceDataType_-/>'") {
        rs => rs.update(Map("data" -> renamer.rename(rs.getString("data"))))
      }
    }
  }
}
