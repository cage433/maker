package starling.schemaevolution


import java.sql.Connection
import system.{PatchUtils, Patch}
import starling.utils.sql.QueryBuilder._
import starling.utils.{Log, StackTraceToString}
import starling.instrument.{DeletedInstrument, ErrorInstrument, FXForward, Future}
import starling.richdb.{RichResultSetRowFactory, RichDB}
import starling.props.Props
import starling.tradestore.TradeStore
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch4_ProductionBug_FXForwards extends Patch {


  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
//    val sql = (select ("*")
//              from "Trade"
//              where ("instrument" neq DeletedInstrument.name)
//            )
//
//    starling.inTransaction {
//      writer => {
//        starling.query(sql) {
//          rs => {
//            try {
//              TradeStore.tradeFromRow(rs)
//            }
//            catch {
//              case e => {
//                val instrument = new ErrorInstrument(StackTraceToString.string(e))
//                val et = TradeStore.tradeFromRow(portfolio, rs, instrument)
//                //            Log.warn("Error loading trade: " + et.trade.tradeID, e)
//                val id = rs.getInt("id")
//                writer.update("Trade", Map("instrument" -> ErrorInstrument.name) ++ instrument.details, ("id" eql id))
//              }
//            }
//          }
//        }
//      }
//    }
    ;
  }
}