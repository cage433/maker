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

/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 24/06/11
 * Time: 11:02
 * To change this template use File | Settings | File Templates.
 */

/**
 * Valuation services
 */
trait IValuationService {
  def valueAllQuotas(maybeSnapshotIdentifier : Option[String] = None): Either[List[Either[List[CostsAndIncomeQuotaValuation], String]], String]
}

/**
 * Valuation service implementations
 */
class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends TacticalRefData(props: Props) with IValuationService {
  def valueAllQuotas(maybeSnapshotIdentifier : Option[String] = None): Either[List[Either[List[CostsAndIncomeQuotaValuation], String]], String] = {
    try {
      val snapshotID = if (maybeSnapshotIdentifier.isDefined) maybeSnapshotIdentifier else mostRecentSnapshotIdentifier()
      println(snapshotID)
      if (! snapshotID.isDefined)
        throw new IllegalStateException("No market data snapshots")
      val sw = new Stopwatch()

      val edmTradeResult = titanGetEdmTradesService.getAll()
      println("Got Edm Trade result " + edmTradeResult.cached + ", took " + sw)

      if (!edmTradeResult.cached)
        Right("Trade Management building trade cache. This takes a few minutes")
      else {
        println("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
        val env = environment(snapshotStringToID(snapshotID.get))
        val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotID.get) _

        val edmTrades: List[EDMPhysicalTrade] = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
        sw.reset()
        val valuations = edmTrades.map(tradeValuer)
        println("Valuation took " + sw)
        val (errors, worked) = valuations.partition(_ match {
          case Right(x) => true;
          case _ => false
        })
        println("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
        errors.foreach{case Right(msg) => println(msg); case _ => }
        Left(valuations)
      }

    } catch {
      case ex =>
        Right(ex.getMessage())
        throw ex
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

  private def mostRecentSnapshotIdentifier() : Option[String] = {
    updateSnapshotCache()
    snapshotNameToID.values.toList.sortWith(_>_).headOption.map(_.id.toString)
  }

  def snapshotStringToID(id: String): SnapshotID = {
    snapshotNameToID.getOrElse(id, {
      updateSnapshotCache()
      assert(snapshotNameToID.contains(id), "Snapshot ID " + id + " not found")
      snapshotNameToID(id)
    }
    )
  }

  def snapshotIDsForDay(day: Day): List[String] = {
    snapshotNameToID.filter {
      case (id, snapshotID) => snapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals)
    }.map(_._1).toList
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
