package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch75_RenameSpreadPeriodLabel extends Patch {
  def patchDescription = "Renames the SpreadPeriodLabel to OptionalPeriodLabel"

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val sql = "select layout from PivotLayouts"
    writer.queryForUpdate(sql) {
      rs => {
        val layout = rs.getString("layout")
        val renamedLayout = layout.replaceAll("SpreadPeriodLabel", "OptionalPeriodLabel")
        val newLayout = renamedLayout.replaceAll("""<errorString class="scala.None\$"/>""", "")
        rs.update(Map("layout" -> newLayout))
      }
    }
  }
}
