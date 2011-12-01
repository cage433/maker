package starling.manual

import starling.daterange.Day
import starling.props.PropsHelper
import starling.services.StarlingInit
import starling.auth.AuthHandler
import starling.manager.Broadcaster
import starling.gui.api.{EAIDeskInfo, Desk, PricingGroup, MarketDataSelection}
import starling.db.DB._
import starling.db.DB

object RemoveBookCloses {
  def main(args: Array[String]) {
    val eaiTables = Desk.eaiDesks.map {
      case Desk(_, _, Some(i: EAIDeskInfo)) => "EAITrade_Book_" + i.book
    }
    val props = PropsHelper.defaultProps

    val starlingDB = DB(props.StarlingDatabase())

    // how many to delete
    val deleteLast = 70

    val ts = starlingDB.queryWithResult("select top(:num) tradeTimestamp from closeddesks order by id desc", Map("num" -> deleteLast)) {
      rs => rs.getTimestamp("tradeTimestamp")
    }
    val toDelete = ts.last

    starlingDB.inTransaction {
      writer => {
        writer.update("delete from ClosedDesks where tradeTimestamp >= :ts", Map("ts" -> toDelete))

        eaiTables.map {
          table => {
            writer.update("delete from " + table + " where timestamp >= :ts", Map("ts" -> toDelete))
            writer.update("update " + table + " set timestampTo_cache = null, nextVersionID_cache = null where timestampTo_cache >= :ts", Map("ts" -> toDelete))
          }
        }
      }
    }
  }
}