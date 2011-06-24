package starling.services.rpc.marketdata

import starling.props.Props
import starling.utils.Stopwatch
import starling.instrument.{PhysicalMetalForward, CostsAndIncomeQuotaValuation}
import com.trafigura.edm.trades.PhysicalTrade
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.gui.api.MarketDataIdentifier._
import starling.curves.{ClosesEnvironmentRule, Environment}
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import java.lang.Throwable
import org.joda.time.LocalDate

/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 24/06/11
 * Time: 11:02
 * To change this template use File | Settings | File Templates.
 */


/**
 * These are the market data snapshot identifiers as viewed outside of starling.
 * id is unique across all days.
 */
case class TitanSnapshotIdentifier(id : String, observationDay : LocalDate)

/**
 * Valuation services
 */
trait IValuationService {
  def valueAllQuotas(maybeSnapshotIdentifier : Option[String] = None): CostsAndIncomeQuotaValuationServiceResults

  /**
   * Return all snapshots for a given observation day, or every snapshot if no day is supplied
   */
  def marketDataSnapshotIDs(observationDay : Option[LocalDate] = None): List[TitanSnapshotIdentifier]
}

/**
 * Valuation service implementations
 */
case class CostsAndIncomeQuotaValuationServiceResults(snapshotID : String, tradeResults : List[Either[List[CostsAndIncomeQuotaValuation], String]])
object TradeManagamentCacheNotReady extends Throwable

class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends TacticalRefData(props: Props)  {
  def valueAllQuotas(maybeSnapshotIdentifier : Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    val snapshotIDString = maybeSnapshotIdentifier.orElse(mostRecentSnapshotIdentifierBeforeToday()) match {
      case Some(id) => id
      case _ => throw new IllegalStateException("No market data snapshots")
    }
    println(snapshotIDString)
    val sw = new Stopwatch()

    val edmTradeResult = titanGetEdmTradesService.getAll()
    println("Got Edm Trade result " + edmTradeResult.cached + ", took " + sw)

    if (!edmTradeResult.cached)
      throw TradeManagamentCacheNotReady
    else {
      println("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
      val env = environment(snapshotStringToID(snapshotIDString))
      val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

      val edmTrades: List[EDMPhysicalTrade] = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
      sw.reset()
      val valuations = edmTrades.map(tradeValuer)
      println("Valuation took " + sw)
      val (errors, worked) = valuations.partition(_ match {
        case Right(x) => true;
        case _ => false
      })
      println("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
      //errors.foreach{case Right(msg) => println(msg); case _ => }
      CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, valuations)
    }
  }

  private val snapshotNameToID = scala.collection.mutable.Map[String, SnapshotID]()
  val lock = new Object()

  def updateSnapshotCache() {
    lock.synchronized {
      marketDataStore.snapshots().foreach {
        s: SnapshotID =>
          snapshotNameToID += s.id.toString -> s
      }
    }
  }

  private def mostRecentSnapshotIdentifierBeforeToday() : Option[String] = {
    updateSnapshotCache()
    snapshotNameToID.values.toList.filter(_.observationDay < Day.today()).sortWith(_>_).headOption.map(_.id.toString)
  }

  def snapshotStringToID(id: String): SnapshotID = {
    snapshotNameToID.getOrElse(id, {
      updateSnapshotCache()
      assert(snapshotNameToID.contains(id), "Snapshot ID " + id + " not found")
      snapshotNameToID(id)
    }
    )
  }

  /**
   * Return all snapshots for a given observation day, or every snapshot if no day is supplied
   */
  def marketDataSnapshotIDService(observationDay : Option[LocalDate] = None): List[TitanSnapshotIdentifier] = {
    updateSnapshotCache()
    snapshotNameToID.values.filter {
      starlingSnapshotID =>
        starlingSnapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) && (observationDay.isEmpty || (starlingSnapshotID.observationDay.toJodaLocalDate == observationDay.get))
    }.map{
      starlingSnapshotID => TitanSnapshotIdentifier(starlingSnapshotID.id.toString, starlingSnapshotID.observationDay.toJodaLocalDate)
    }.toList
  }

  private def environment(snapshot: SnapshotID): Environment = {
    val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshot.marketDataSelection, snapshot.version))
    ClosesEnvironmentRule.createEnv(snapshot.observationDay, reader).environment
  }
}

object ValuationService {

  // factory for creating client proxy for valuation services
  def create() : IValuationService = null
  def create(serviceUri : String) : IValuationService = null
}
