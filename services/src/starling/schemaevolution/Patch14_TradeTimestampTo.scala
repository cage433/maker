package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import java.sql.{Connection, Statement, ResultSet}
import starling.props.Props
import starling.daterange.Timestamp
import starling.dbx.QueryBuilder._
import starling.services.StarlingInit

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 01-Apr-2010
 * Time: 15:56:08
 * To change this template use File | Settings | File Templates.
 */

class Patch14_TradeTimestampTo extends Patch {
  case class Row(id:Int, tradeID:Int, timestamp:Timestamp)
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    List("TrinityTrade", "GalenaTrade", "EAITrade").foreach { table =>
      writer.update("alter table " + table + " add timestampTo_cache datetime")
      writer.update("alter table " + table + " add nextVersionId_cache int")
    }
    List("TrinityTrade", "GalenaTrade").foreach { table =>
      writer.update("ALTER TABLE " + table + " DROP CONSTRAINT PK_"+table+"_id")
      writer.update("ALTER TABLE " + table + " ADD CONSTRAINT PK_" + table + "_id PRIMARY KEY NONCLUSTERED (id)")
      writer.update("CREATE CLUSTERED INDEX NUC_" + table + " on " + table + "(timestampto_cache, instrument, portfolio, tradeid)")
    }

    List("TrinityTrade", "GalenaTrade", "EAITrade").foreach { table=> updateCacheFieldsForTable(starling, writer, table) }
  }

  def updateCacheFieldsForTable(starling: RichDB, writer: DBWriter, table:String) {
    println("Updating table " + table)
    var lastRow:Option[Row] = None
    val count = starling.queryWithOneResult("select count(id) c from " + table, Map()) { rs=> rs.getInt("c") }.get
    println( count + " rows to update")
    var rr = 0

//    starling.query("select id, tradeID, timestamp, timestampTo_cache, nextVersionId_cache from " + table, Map()) {
    starling.query("select id, tradeID, timestamp, timestampTo_cache, nextVersionId_cache from " + table + " order by tradeID, id desc", Map()) {
      rs=> {
        val row = Row(rs.getInt("id"), rs.getInt("tradeID"), rs.getTimestamp("timestamp"))
        lastRow match {
          case Some(lr) if lr.tradeID==row.tradeID => {
            writer.update(table, Map("timestampTo_cache"->lr.timestamp, "nextVersionId_cache"->lr.id), ("id" eql row.id))
          }
          case _ => {}
        }
        lastRow = Some(row)
        rr=rr+1
        if (rr%25000==0) {
          println(rr + " / " + count)
        }
      }
    }
  }

  def patchDescription = "Adds timestampTo_cache and nextVersionId_cache to all *Trade tables"
}