package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch85_AddExcelEnabledToReports extends Patch {
  def patchDescription = "Adds whether excel is enabled to saved reports"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {
    val updateText = """<boolean>false</boolean>"""

    writer.queryForUpdate("select report from UserReports") {
      rs => {
        val report = rs.getString("report")
        val startIndex = report.indexOf("</x></pnl>")
        if (startIndex != -1) {
          val (start, end) = report.splitAt(startIndex)
          val newReport = start + updateText + end
          rs.update(Map("report" -> newReport))
        }
      }
    }
  }
}
