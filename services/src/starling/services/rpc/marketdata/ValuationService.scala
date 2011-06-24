package starling.services.rpc.marketdata

import starling.props.Props
import starling.instrument.PhysicalMetalForward
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.gui.api.MarketDataIdentifier._
import starling.curves.{ClosesEnvironmentRule, Environment}
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import org.joda.time.LocalDate
import starling.services.StarlingInit
import com.trafigura.valuationservice._
import starling.utils.{Log, Stopwatch}


/**
 * Valuation service implementations
 */

class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends TacticalRefData(props: Props) with ValuationServices {
  def log(msg : String) = Log.info("ValuationService: " + msg)

  def valueAllQuotas(maybeSnapshotIdentifier : Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    log("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = maybeSnapshotIdentifier.orElse(mostRecentSnapshotIdentifierBeforeToday()) match {
      case Some(id) => id
      case _ => throw new IllegalStateException("No market data snapshots")
    }
    log("Actual snapshot ID " + snapshotIDString)
    val sw = new Stopwatch()

    val edmTradeResult = titanGetEdmTradesService.getAll()
    log("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)

    if (!edmTradeResult.cached)
      throw TradeManagamentCacheNotReady
    else {
      log("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
      val env = environment(snapshotStringToID(snapshotIDString))
      val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

      val edmTrades: List[EDMPhysicalTrade] = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
      sw.reset()
      val valuations = edmTrades.map{trade => (trade.tradeId, tradeValuer(trade))}.toMap
      log("Valuation took " + sw)
      val (errors, worked) = valuations.values.partition(_ match {
        case Right(_) => true;
        case Left(_) => false
      })
      log("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
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
  def marketDataSnapshotIDs(observationDay : Option[LocalDate] = None): List[TitanSnapshotIdentifier] = {
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

object ValuationService extends Application{
  lazy val vs = StarlingInit.devInstance.valuationService

  val valuations = vs.valueAllQuotas()
  //vs.marketDataSnapshotIDs().foreach(println)

}
