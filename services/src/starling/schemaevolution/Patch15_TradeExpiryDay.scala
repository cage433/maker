package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.instrument.{TradeableType, InstrumentType}
import starling.services.StarlingInit

/**
 *
 */

class Patch15_TradeExpiryDay extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starlingDB: RichDB, writer: DBWriter) = {
    List("TrinityTrade", "GalenaTrade", "EAITrade").foreach { table =>
      writer.update("alter table " + table + " add expiryDay_cache datetime")
      populateExpiryDayField(starlingDB, writer, table)
    }
  }

  def populateExpiryDayField(starlingDB: RichDB, writer: DBWriter, table:String) {
    println("Updating table " + table)
    val count = starlingDB.queryWithOneResult("select count(id) c from " + table, Map()) { rs=> rs.getInt("c") }.get
    println( count + " rows to update")
    var rr = 0

    starlingDB.query("select * from " + table, Map()) {
      row => {
        val id = row.getInt("id")
        val instrumentName = row.getString("Instrument")
        val instrumentType = TradeableType.fromName(instrumentName)
        val instrument = instrumentType.createTradeable(row)
        import starling.dbx.QueryBuilder._
        writer.update(table, Map("expiryDay_cache"->(instrument.expiryDay match {
          case Some(day) => day
          case None => null
        })), ("id" eql id))

        rr=rr+1
        if (rr%10000==0) {
          println(rr + " / " + count + "  " + ((rr.toLong/count)*100)+"%")
        }
      }
    }
  }

  def patchDescription = "Adds expiryDay_cache to all *Trade tables"
}