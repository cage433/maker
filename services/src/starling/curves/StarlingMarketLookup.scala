package starling.curves

import starling.db.DB
import starling.calendar.BusinessCalendars
import starling.market._
import starling.utils.sql.QueryBuilder._
import starling.utils.Log
import java.util.concurrent.atomic.AtomicBoolean

class StarlingMarketLookup(db: DB, businessCalendars: BusinessCalendars, expiryRules: FuturesExpiryRules) extends MarketLookup {
  /**
   * map of eaiquoteid to index
   */
  lazy val table: List[Either[Market, Index]] = load

  val loading = new AtomicBoolean(false)

  lazy val allMarkets = table.flatMap {
    case Left(m: Market) => Some(m)
    case _ => None
  }
  lazy val allIndexes = table.flatMap {
    case Right(m) => Some(m)
    case _ => None
  }

  private def load = {
    assert(loading.compareAndSet(false, true), "Already loading/loaded")
    Log.infoWithTime("Loading markets ") {
      val parser = new MarketParser(businessCalendars, expiryRules)
      val lines = db.queryWithResult((select("*") from ("Markets") orderBy ("forwardMarket" asc))) {
        rs => {
          val map = rs.asMap.map {
            case (name, entry) => name.toLowerCase -> (if (entry == null) "" else entry.toString.trim)
          }
          new MarketParser.Line {
            def get(name: String) = map(name.toLowerCase)
          }
        }
      }
      Log.info("Loading " + lines.size + " markets.")
      parser.fromLines(lines)
    }
  }
}