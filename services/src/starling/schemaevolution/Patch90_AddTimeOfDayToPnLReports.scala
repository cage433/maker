package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.Patch
import starling.services.StarlingInit

class Patch90_AddTimeOfDayToPnLReports extends Patch {
  def patchDescription = "Adds pnl time of day to the saved reports"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select report from UserReports"
    val searchFor = """<pnl class="scala.Some">"""
    writer.queryForUpdate(sql) {
      rs => {
        val report = rs.getString("report")
        val index = report.indexOf(searchFor)
        if (index != -1) {
          val bool = """</boolean>"""
          val boolIndex = report.indexOf(bool, index)
          val (start, end) = report.splitAt(boolIndex + bool.length())
          val insert = """<starling.daterange.TimeOfDay><name>End of Day</name><shortName>EoD</shortName></starling.daterange.TimeOfDay>"""
          val newReport = start + insert + end
          rs.update(Map("report" -> newReport))
        }
      }
    }
  }
}
