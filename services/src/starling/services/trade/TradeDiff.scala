package starling.services.trade

import starling.instrument._
import starling.utils.CollectionUtils._
import starling.tradestore.eai.EAITradeAttributes
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.daterange.{Day, Timestamp}
import starling.utils.Log
import starling.pivot.{UnionPivotTableDataSource, FieldDetailsGroup}
import starling.tradestore.{TradeSet, TradeAndFields}

/**
 * Compares trades in aTrades to bTrades.
 *
 * Ignores trade ids. This is specifically designed for EAI/Excel trade comparisons.
 */
class TradeDiff(val fields: List[FieldDetailsGroup], aTrades: List[TradeAndFields], bTrades: List[TradeAndFields]) {
  import TradeDiff._

  lazy val matches: List[(TradeAndFields, TradeAndFields)] = diffs.matches
  lazy val nearMatches: List[(TradeAndFields, TradeAndFields)] = diffs.nearMatches
  lazy val unmatched: List[TradeAndFields] = diffs.unmatched
  lazy val aTradesWithLegs: List[TradeAndFields] = aTrades.flatMap(multiLeg)
  lazy val bTradesWithLegs: List[TradeAndFields] = bTrades.flatMap(multiLeg)

  private def multiLeg(trade: TradeAndFields): List[TradeAndFields] = trade.trade.tradeable match {
    case i: MultiLeg => i.legs.map(leg => trade.withNewInstrument(leg))
    case _ => List(trade)
  }

  private case class Diff(matches: List[(TradeAndFields, TradeAndFields)], nearMatches: List[(TradeAndFields, TradeAndFields)], unmatched: List[TradeAndFields])

  private def lev(a: Array[Any], b: Array[Any]): Int = {
    val dist = levenshtein(a, b)
    dist
  }

  private lazy val diffs = {
    val distances = aTradesWithLegs.flatMap {
      aTrade => {
        val a = expand(aTrade)
        bTradesWithLegs.map {
          bTrade => {
            val b = expand(bTrade)
            val dist = lev(a, b)
            (aTrade, bTrade, dist)
          }
        }
      }
    }

    var done = Set[TradeAndFields]()
    val same = distances.flatMap {
      case (a, b, 0) if !done.contains(a) && !done.contains(b) => {
        done ++= Set(a, b)
        Some((a, b))
      }
      case _ => None
    }
    val near = distances.flatMap {
      case (a, b, 1) if !done.contains(a) && !done.contains(b) => {
        done ++= Set(a, b)
        Some((a, b))
      }
      case _ => None
    } ++ distances.flatMap {
      case (a, b, 2) if !done.contains(a) && !done.contains(b) => {
        done ++= Set(a, b)
        Some((a, b))
      }
      case _ => None
    }

    val unmatchedTrades = ((aTradesWithLegs ++ bTradesWithLegs).toSet -- done).toList
    Diff(same, near, unmatchedTrades)
  }
}

object TradeDiff {
  def apply(eaiTrades: TradeSet, from: Timestamp, to: Timestamp, intradayTimestamp: Timestamp, excelTrades: List[TradeSet]) = {
    val tradeChanges = eaiTrades.tradeChanges(from, to, from.day.startOfFinancialYear)
    val aTrades = tradeChanges.created ::: tradeChanges.movedIn ::: tradeChanges.amended.map(_._2)

    val tradesAndDetails = excelTrades.map(ts => ts.readAll(intradayTimestamp))
    val bTrades = tradesAndDetails.flatMap(_._2)
    val bFields = tradesAndDetails.flatMap(_._1)
    val fields = UnionPivotTableDataSource.unionFieldDetails(tradeChanges.fields, bFields)
    new TradeDiff(fields, aTrades, bTrades)
  }

  /**
   * expands a trade into an array of params
   */
  def expand(taf: TradeAndFields): Array[Any] = {
    val t = taf.trade
    val instrumentDetails = t.tradeable.tradeableType.name :: t.tradeable.tradeableDetails.values.toList
    val attrs = t.attributes match {
      case ta: EAITradeAttributes => {
        List(ta.dealID.id, ta.strategyID.id, ta.clearingHouse.toLowerCase)
      }
      case ta: IntradayTradeAttributes => {
        val dealID = ta.dealID.map(_.id).getOrElse(-1)
        val strategyID = ta.strategyID.map(_.id).getOrElse(-1)
        val clearer = ta.clearingHouse.toLowerCase
        List(dealID, strategyID, clearer)
      }
    }

    val otherDetails = t.counterParty :: attrs
    val importantDetails = List(t.tradeDay, t.costs)
    // compare List(List(otherdetails), "instrument", "market" ...)
    // in other words we don't want a ':::' between other details and the import stuff in the line below.
    // this means that any of the other details being wrong just gives in total 1 negative point against a trade comparison.
    (otherDetails :: (importantDetails ::: instrumentDetails)).toArray
  }
}

