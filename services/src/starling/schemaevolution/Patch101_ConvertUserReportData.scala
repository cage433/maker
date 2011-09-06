package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.instrument.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import collection.SortedMap
import scala.Some
import starling.pivot._
import model.CollapsedState
import starling.utils.sql.PersistAsBlob
import collection.immutable.SortedSet
import starling.gui.api._
import starling.daterange.TimeOfDay

class Patch101_ConvertUserReportData extends Patch {


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

        val tradeVersionOffSetOrLive = fields.getFieldValue("tradeVersionOffSetOrLive").get.asInstanceOf[Int]
        val newTradeVersionOffSetOrLive = if (tradeVersionOffSetOrLive.abs <= 1) {
          Right(true)
        } else {
          Left(tradeVersionOffSetOrLive)
        }


        val liveOnOffSet = fields.getFieldValue("liveOnOffSet").get.asInstanceOf[Int]
        val newLiveOnOffSet = if (liveOnOffSet.abs > 20) {
          // Probably start of financial year.
          Right(true)
        } else {
          Left(liveOnOffSet)
        }

        val pnl = fields.getFieldValue("pnl").get.asInstanceOf[Option[(Int,Int,Boolean,TimeOfDay)]]
        val newPnL = pnl match {
          case None => None
          case Some(pnl0) => {
            val newBit = if (pnl0._2.abs <= 1) {
              Right(true)
            } else {
              Left(pnl0._2)
            }
            Some(pnl0._1, newBit, pnl0._3, pnl0._4)
          }
        }

        UserReportData(ts, mds, envModifiers, rO, envRule, valOff, valTOD, thetaOff, thetaDayTimeOfDay, newTradeVersionOffSetOrLive, newLiveOnOffSet, newPnL)
      }
    },
    Map("tradeVersionOffSetOrLive" -> classOf[Int], "liveOnOffSet" -> classOf[Int], "pnl" -> classOf[Option[(Int,Int,Boolean,TimeOfDay)]])
  ))

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select report from UserReports"
    writer.queryForUpdate(sql) {
      rs => {
        val report = rs.getString("report")
        val newReport = convertingXStream.fromXML(report)
        rs.update(Map("report" -> new PersistAsBlob(newReport)))
      }
    }
  }
}