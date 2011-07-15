package starling.schemaevolution

import starling.schemaevolution.system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import scala.util.matching.Regex
import starling.utils.ImplicitConversions._
import starling.utils.Log

class Patch106_AddBooleanToHashMapInUserSettings extends Patch {
  val regex = new Regex("""<int>\d+</int>\s*<int>\d+</int>""")
  def removeBitmaps(starling : RichDB, table : String, column : String){
    Log.info("Adding booleans to XStream hash map for " + table + ", " + column)
    starling.inTransaction {
      writer: DBWriter => {
      {
        val query = "select " + column + " from " + table 
            writer.queryForUpdate(query) {
              rs => {
                val text: String = rs.getString(column)
                val newText = regex.replaceAllIn(text, m => m.matched + "<boolean>false</boolean>")
                rs.update(Map(column -> newText))
              }
            }
          }
      }
    }
  }
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    removeBitmaps(starling, "usersettings", "settings")
  }
}
