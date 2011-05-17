package starling.schemaevolution

import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import collection.mutable.{ListBuffer, HashMap}
import starling.pivot.PivotLayout
import starling.services.StarlingInit

class Patch81_AddColumnsToLayoutTable extends Patch {
  def patchDescription = "Adds layoutType and associatedReport to the PivotLayouts table, and remove layoutName from UserReports"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch81_AddColumnsToLayoutTable.sql"))

    // Try to guess which layouts are associated to which reports.
    val userToReportLayout = new HashMap[String,ListBuffer[(String,String)]]()

    starling.queryWithResult("select starlingUser, reportName, layoutName from UserReports") {
      rs => {
        val user = rs.getString("starlingUser")
        val reportName = rs.getString("reportName")
        val layoutName = rs.getString("layoutName")

        userToReportLayout.getOrElseUpdate(user, new ListBuffer[(String,String)]()) += ((reportName, layoutName))
      }
    }

    writer.queryForUpdate("select * from PivotLayouts") {
      rs => {
        val user = rs.getString("starlingUser")
        if (userToReportLayout.contains(user)) {
          val thisLayout = rs.getString("layoutName")
          val reports = userToReportLayout(user).filter{case (_,layout) => layout == thisLayout}.map(_._1)
          if (reports.nonEmpty) {
            val reportString = reports.mkString(PivotLayout.AssociatedReportsDelimiter)
            rs.update(Map("associatedReport" -> reportString))
          }
        }
      }
    }

    writer.update("alter table UserReports drop column layoutName")
  }
}
