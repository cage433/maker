package starling.schemaevolution

import starling.schemaevolution.system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import scala.util.matching.Regex
import starling.utils.ImplicitConversions._
import starling.utils.Log

class Patch102_RemoveBitmapsFromXStream extends Patch {
  val regex = new Regex("""<bitmap_-\d+>\d+</bitmap_-\d+>""")
  def removeBitmaps(starling : RichDB, table : String, column : String){
    Log.info("Removing bitmaps from " + table + ", " + column)
    var i = 0
    starling.inTransaction {
      writer: DBWriter => {
      {
        val query = "select " + column + " from " + table + " where " + column + " like '%<bitmap%'" 
            writer.queryForUpdate(query) {
              rs => {
                i = i + 1
                if (i % 5000 == 0)
                  Log.info("Done row " + i)
                val text: String = rs.getString(column)
                val newText = regex.replaceAllIn(text, "")
                rs.update(Map(column -> newText))
              }
            }
          }
      }
    }
  }
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    removeBitmaps(starling, "MarketData", "data")
    removeBitmaps(starling, "PivotLayouts", "layout")
  }
}
