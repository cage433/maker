package starling.systemofrecord

import starling.instrument._
import java.lang.String
import starling.richdb.{RichDB, RichResultSetRow}
import starling.utils._
import starling.dbx.SqlRenderer
import starling.instrument.Trade
import starling.dbx.QueryBuilder._
import starling.dbx.Clause
import starling.dbx.Query



trait SystemOfRecord {
  def allTrades : Seq[Trade]
}

abstract class SystemOfRecordBackedByADatabase(externalDB : RichDB) extends SystemOfRecord{
  protected def readers: List[InstrumentReader]
  /**
   * f is called with each Trade read in.
   * Returns a unique error count and a set of unique errors
   */
  protected def allTrades(query : String, parameters : Map[String, Any]) : Seq[Trade] = {
    var uniqueErrors = Set[String]()
    var trades : List[Trade] = Nil
    externalDB.query(query, parameters) {
      rs => {
        try {
          val nullTrade = createNullTrade(rs)

          val trade = try {
            val ins = createInstrument(rs)
            addCostsToTrade(nullTrade.copyWithInstrument(ins), rs)
          } catch {
            case e => {
              if (!uniqueErrors.contains(e.toString)) {
                Log.warn("Error Trade: " + e + " " + nullTrade.tradeID + " " + nullTrade.tradeDay)
                uniqueErrors = uniqueErrors + e.toString
              }
              nullTrade.copyWithInstrument(ErrorInstrument(StackTraceToString.string(e).trim))
            }
          }
          trades = trade :: trades
        }
        catch {
          case e:AssertionError => throw e
          case e => Log.error("Problem creating empty trade: " + rs, e)
        }
      }
    }
    trades
  }

  protected def allTrades(q: Query) : Seq[Trade] = {

    val renderer: SqlRenderer = new SqlRenderer()
    val sql = renderer.render(q)
    allTrades(sql.query, sql.parameters)
  }

  protected def createInstrument(rs: RichResultSetRow): Tradeable = {
    try {
      readers.filter(r => r.canHandle(rs)) match {
        case reader :: Nil => reader.create(rs)
        case Nil => throw new Exception("No readers matched: " + rs)
        case r => throw new Exception("Too many readers (" + r + ") matched: " + rs)
      }
    }
    catch {
      case m:MatchError => throw new Exception("A broken reader is throwing a match error instead of returning false", m)
    }
  }

  protected def createNullTrade(rs: RichResultSetRow) : Trade

  protected def addCostsToTrade(trade: Trade, rs: RichResultSetRow) : Trade = trade
}
