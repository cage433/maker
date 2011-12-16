package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.instrument.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import starling.gui.pages.ValuationParametersBookmark
import starling.utils.sql.PersistAsBlob
import starling.gui.api.{ReportSpecificChoices, UserReportData, TradeIDLabel}

class Patch158_UpdateValuationParametersBookmark extends Patch {
  val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[ValuationParametersBookmark],
    new Reader {
      def create(fields:Fields) = {
        val tradeID = fields.getFieldValue("tradeID").get.asInstanceOf[TradeIDLabel]
        val userReportData = fields.getFieldValue("userReportData").get.asInstanceOf[UserReportData]

        ValuationParametersBookmark(tradeID, userReportData, ReportSpecificChoices(), true)
      }
    }
  ))

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select bookmark from Bookmarks"
    writer.queryForUpdate(sql) {
      rs => {
        val bookmark = rs.getString("bookmark")
        val newBookmark = convertingXStream.fromXML(bookmark)
        rs.update(Map("bookmark" -> new PersistAsBlob(newBookmark)))
      }
    }
  }
}