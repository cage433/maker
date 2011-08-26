package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import starling.daterange.TimeOfDay
import collection.immutable.SortedSet
import starling.gui.api._
import starling.utils.sql.PersistAsBlob

class Patch93_EnvironmentRuleInReports extends Patch {

  /*val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
      StarlingXStream.createXStream,
      classOf[UserReportData],
      new Reader(){
        def create(fields: Fields) = {
          val tradeSelection:TradeSelection = fields.getFieldValue("tradeSelection").get.asInstanceOf[TradeSelection]
          val marketDataSelection:MarketDataSelection = fields.getFieldValue("marketDataSelection").get.asInstanceOf[MarketDataSelection]
          val environmentModifiers:collection.immutable.SortedSet[EnvironmentModifierLabel] = fields.getFieldValue("environmentModifiers").get.asInstanceOf[SortedSet[EnvironmentModifierLabel]]
          val reportOptions:ReportOptions = fields.getFieldValue("reportOptions").get.asInstanceOf[ReportOptions]

          val environmentRule = fields.getFieldValue("environmentRule") match {
            case Some(rule) => rule.asInstanceOf[EnvironmentRuleLabel]
            case None => {
              val marketDataDayOffset = fields.getFieldValue("marketDataDayOffset").get.asInstanceOf[Int]
              if (marketDataDayOffset == -1) EnvironmentRuleLabel.RealTime else EnvironmentRuleLabel.COB
            }
          }

          val valuationDayOffset:Int = fields.getFieldValue("valuationDayOffset").get.asInstanceOf[Int]
          val valuationDayTimeOfDay:TimeOfDay = fields.getFieldValue("valuationDayTimeOfDay").get.asInstanceOf[TimeOfDay]
          val thetaDayOffset:Int = fields.getFieldValue("thetaDayOffset").get.asInstanceOf[Int]
          val thetaDayTimeOfDay:TimeOfDay = fields.getFieldValue("thetaDayTimeOfDay").get.asInstanceOf[TimeOfDay]
          val tradeVersionOffSetOrLive:Int = fields.getFieldValue("tradeVersionOffSetOrLive").get.asInstanceOf[Int]
          val liveOnOffSet:Int = fields.getFieldValue("liveOnOffSet").get.asInstanceOf[Int]
          val pnl:Option[(Int,Int,Boolean,TimeOfDay)] = fields.getFieldValue("pnl").get.asInstanceOf[Option[(Int,Int,Boolean,TimeOfDay)]]

          new UserReportData(
            tradeSelection, marketDataSelection, environmentModifiers, reportOptions, environmentRule, valuationDayOffset,
            valuationDayTimeOfDay, thetaDayOffset, thetaDayTimeOfDay, tradeVersionOffSetOrLive, liveOnOffSet, pnl
          )
        }
      },
      Map("marketDataDayOffset" -> classOf[Int])
    ))*/

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {

    throw new Exception("I've changed the format of UserReportData and can't be bothered changing this evolution.")

    /*writer.queryForUpdate("select report from UserReports") {
      rs => {
        val reportText = rs.getString("report")
        println("ReportText\n" + reportText + "\n\n")
        val newReport = convertingXStream.fromXML(reportText)
        rs.update(Map("report" -> AnObject(newReport)))
      }
    }*/
  }
}