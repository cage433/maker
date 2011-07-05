package starling.curves

import starling.db.DB
import starling.calendar.BusinessCalendars
import starling.market._
import starling.utils.sql.QueryBuilder._
import starling.utils.Log

class StarlingMarketLookup(db: DB, businessCalendars: BusinessCalendars, expiryRules: FuturesExpiryRules) extends MarketLookup {
  /**
   * map of eaiquoteid to index
   */
  lazy val table: List[Either[CommodityMarket, Index]] = load

  lazy val allFuturesMarkets = table.flatMap {
    case Left(m: FuturesMarket) => Some(m)
    case _ => None
  }
  lazy val allIndexes = table.flatMap {
    case Right(m) => Some(m)
    case _ => None
  }

  private def load = {
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