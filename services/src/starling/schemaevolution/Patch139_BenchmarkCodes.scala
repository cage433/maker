package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder
import starling.utils.ImplicitConversions._
import QueryBuilder._
import xml.transform.{RuleTransformer, RewriteRule}
import xml.{Node, Utility, Elem, Text}

class Patch139_BenchmarkCodes extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    ChangeMarketDataValueKeys.fixMarketDataValueKey(starling, writer, Fix)
  }
  object Fix extends RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case <entry><string>Country Code</string>{value}</entry> => <entry><string>Country</string>{value}</entry>
      case <entry><string>Area Code</string>{value}</entry> => <entry><string>Area</string>{value}</entry>
      case <entry><string>Grade Code</string>{value}</entry> => <entry><string>Grade</string>{value}</entry>
      case o => o
    }
  }
}
object ChangeMarketDataValueKeys {

  def fixMarketDataValueKey(starling: RichDB, writer: DBWriter, rule:RewriteRule) {
    fix(starling, writer, "MarketDataValueKey", "valueKey", rule)
  }

  def fixMarketDataExtendedKey(starling: RichDB, writer: DBWriter, rule:RewriteRule) {
    fix(starling, writer, "MarketDataExtendedKey", "marketDataKey", rule)
  }

  def fix(starling: RichDB, writer: DBWriter, table:String, xmlColumn:String, rule:RewriteRule) {
    val transformer = new RuleTransformer(rule)
    var fixCount = 0
    starling inTransaction { writer => {
      starling.query("select * from " + table) {
        rs => {
          val id = rs.getInt("id")
          val data = Utility.trimProper(rs.getXML(xmlColumn)).toList.head.asInstanceOf[Elem]
          val fixed = transformer(data)
          if (data != fixed) {
            fixCount+=1
            writer.update(table, Map(xmlColumn->fixed), ("id" eql id))
          }
        }
      }
    }}
    println("Fixed " + fixCount + " " + table + " rows")
  }
}
