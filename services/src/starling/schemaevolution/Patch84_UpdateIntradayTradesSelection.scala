package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.utils.StarlingXStream
import starling.collection.TreeSelection
import starling.utils.sql.PersistAsBlob
import xstream.{Fields, Reader, MapBasedConverter}
import starling.services.StarlingInit
import starling.gui.api._
import collection.SortedSet
import starling.daterange.{TimeOfDay, Timestamp}

class Patch84_UpdateIntradayTradesSelection extends Patch {
  def patchDescription = "Updates the intra day selection that is stored in the report parameters and user settings"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {
    
    throw new Exception("I've changed the format of UserReportData and can't be bothered changing this evolution.")

    /*val convertingXStream = StarlingXStream.createXStream

    convertingXStream.registerConverter(new MapBasedConverter(
        StarlingXStream.createXStream,
        classOf[UserReportData],
        new Reader(){
          def create(fields: Fields) = {
            val tradeSelection:TradeSelection = fields.getFieldValue("tradeSelection").get.asInstanceOf[TradeSelection]
            val marketDataSelection:MarketDataSelection = fields.getFieldValue("marketDataSelection").get.asInstanceOf[MarketDataSelection]
            val environmentModifiers:collection.immutable.SortedSet[EnvironmentModifierLabel] = fields.getFieldValue("environmentModifiers").get.asInstanceOf[scala.collection.immutable.SortedSet[EnvironmentModifierLabel]]
            val reportOptions:ReportOptions = fields.getFieldValue("reportOptions").get.asInstanceOf[ReportOptions]
            val marketDataDayOffset:Int = fields.getFieldValue("marketDataDayOffset").get.asInstanceOf[Int]
            val valuationDayOffset:Int = fields.getFieldValue("valuationDayOffset").get.asInstanceOf[Int]
            val valuationDayTimeOfDay:TimeOfDay = fields.getFieldValue("valuationDayTimeOfDay").get.asInstanceOf[TimeOfDay]
            val thetaDayOffset:Int = fields.getFieldValue("thetaDayOffset").get.asInstanceOf[Int]
            val thetaDayTimeOfDay:TimeOfDay = fields.getFieldValue("thetaDayTimeOfDay").get.asInstanceOf[TimeOfDay]
            val tradeVersionOffSetOrLive:Int = fields.getFieldValue("tradeVersionOffSetOrLive").get.asInstanceOf[Int]
            val liveOnOffSet:Int = fields.getFieldValue("liveOnOffSet").get.asInstanceOf[Int]
            val pnl:Option[(Int,Int,Boolean,TimeOfDay)] = fields.getFieldValue("pnl").get.asInstanceOf[Option[(Int,Int,Boolean,TimeOfDay)]]

            val environmentRule = if (marketDataDayOffset == -1) EnvironmentRuleLabel.RealTime else EnvironmentRuleLabel.COB

            new UserReportData(
              tradeSelection, marketDataSelection, environmentModifiers, reportOptions, environmentRule, valuationDayOffset,
              valuationDayTimeOfDay, thetaDayOffset, thetaDayTimeOfDay, tradeVersionOffSetOrLive, liveOnOffSet, pnl
            )
          }
        },
        Map("marketDataDayOffset" -> classOf[Int])
      ))

    def names(treeSelection:TreeSelection[Timestamp]): List[String] = {
      var res = List[String]()
      treeSelection.selection map {
        node => {
          treeSelection.tree.traverse(node) {
            n => {
              if(n.value.isDefined) {
                res ::= n.name
              }
            }
          }
        }
      }
      res
    }

    convertingXStream.registerConverter(new MapBasedConverter(
      StarlingXStream.createXStream,
      classOf[IntradayGroups],
      new Reader(){
        def create(fields: Fields) = {
          val selection = fields.getFieldValue("treeSelection").get.asInstanceOf[TreeSelection[Timestamp]]
          IntradayGroups(names(selection))
        }
      },
      Map("treeSelection" -> classOf[TreeSelection[Timestamp]])
    ))



    starling.inTransaction {
      writer => {
        writer.queryForUpdate("select report from UserReports") {
          rs => {
            val data = rs.getString("report")
            val fixed = convertingXStream.fromXML(data)
            rs.update(Map("report" -> AnObject(fixed)))
          }
        }
        writer.queryForUpdate("select settings from usersettings") {
          rs => {
            val data = rs.getString("settings")
            val fixed = convertingXStream.fromXML(data)
            rs.update(Map("settings" -> AnObject(fixed)))
          }
        }
      }
    }*/
  }
}