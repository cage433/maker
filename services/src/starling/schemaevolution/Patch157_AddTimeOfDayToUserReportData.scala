package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.instrument.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import collection.immutable.SortedSet
import starling.gui.api._
import starling.daterange.TimeOfDay
import starling.utils.sql.PersistAsBlob


class Patch157_AddTimeOfDayToUserReportData extends Patch {
  val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[UserReportData],
    new Reader {
      def create(fields:Fields) = {
        val ts = fields.getFieldValue("tradeSelection").get.asInstanceOf[TradeSelection]
        val mds = fields.getFieldValue("marketDataSelection").get.asInstanceOf[MarketDataSelection]
        val envModifiers = fields.getFieldValue("environmentModifiers").get.asInstanceOf[SortedSet[EnvironmentModifierLabel]]
        val rO = fields.getFieldValue("reportOptions").get.asInstanceOf[ReportOptions]
        val envRule = fields.getFieldValue("environmentRule").get.asInstanceOf[EnvironmentRuleLabel]
        val valOff = fields.getFieldValue("valuationDayOffset").get.asInstanceOf[Int]
        val valTOD = fields.getFieldValue("valuationDayTimeOfDay").get.asInstanceOf[TimeOfDay]
        val thetaOff = fields.getFieldValue("thetaDayOffset").get.asInstanceOf[Int]
        val thetaDayTimeOfDay = fields.getFieldValue("thetaDayTimeOfDay").get.asInstanceOf[TimeOfDay]
        val tradeVOOL = fields.getFieldValue("tradeVersionOffSetOrLive").get.asInstanceOf[Either[Int,Boolean]]
        val liveOnOffSet = fields.getFieldValue("liveOnOffSet").get.asInstanceOf[Either[Int,Boolean]]
        val pnlBla = fields.getFieldValue("pnl").get.asInstanceOf[Option[(Int,Either[Int,Boolean],Boolean,TimeOfDay)]]

        UserReportData(TimeOfDay.EndOfDay, ts, mds, envModifiers, rO, envRule, valOff, valTOD, thetaOff, thetaDayTimeOfDay, tradeVOOL, liveOnOffSet, pnlBla)
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