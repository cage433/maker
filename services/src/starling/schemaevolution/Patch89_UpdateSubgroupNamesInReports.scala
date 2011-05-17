package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.Patch
import starling.services.StarlingInit

class Patch89_UpdateSubgroupNamesInReports extends Patch {
  def patchDescription = "Updates sub group names with complete path in reports"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select distinct(subgroupName) from IntradayTrades"
    val allSubgroupNames = starling.queryWithResult(sql) {
      rs => {
        rs.getString("subgroupName")
      }
    }
    val sql2 = "select starlingUser, report from UserReports"
    writer.queryForUpdate(sql2) {
      rs => {
        val username = rs.getString("starlingUser")
        val report = rs.getString("report")
        val findString = "<subgroups><string>"
        val index = report.indexOf(findString)
        if (index != -1) {
          val (start, end) = report.splitAt(index + findString.length())
          val endIndex = end.indexOf("<")
          val (subgroupName, end2) = end.splitAt(endIndex)

          val newSubgroupName = allSubgroupNames.find(_.endsWith(subgroupName)).get
          val newReport = start + newSubgroupName + end2

          rs.update(Map("report" -> newReport))
        }
      }
    }
  }
}
