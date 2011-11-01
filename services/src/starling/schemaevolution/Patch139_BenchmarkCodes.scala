package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder
import starling.utils.ImplicitConversions._
import QueryBuilder._
import xml.{Utility, Elem, Text}

class Patch139_BenchmarkCodes extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {

    def renameFields(data:Elem) = {
      val fixed = data.child.map {
        case <entry><string>Country Code</string>{value}</entry> => <entry><string>Country</string>{value}</entry>
        case <entry><string>Area Code</string>{value}</entry> => <entry><string>Area</string>{value}</entry>
        case <entry><string>Grade Code</string>{value}</entry> => <entry><string>Grade</string>{value}</entry>
        case o => o
      }
      data.copy(child=fixed)
    }

    var fixCount = 0
    starling inTransaction { writer => {
      starling.query("select * from MarketDataValueKey") {
        rs => {
          val id = rs.getInt("id")
          val data = Utility.trimProper(rs.getXML("valueKey")).toList.head.asInstanceOf[Elem]
          val fixed = renameFields(data)
          if (data != fixed) {
            fixCount+=1
            writer.update("MarketDataValueKey", Map("valueKey"->fixed), ("id" eql id))
          }
        }
      }
    }}
    println("Fixed " + fixCount + " MarketDataValueKey rows")
  }
}
