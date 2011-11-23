package starling.services.rpc.valuation

import starling.curves.Environment
import com.trafigura.services.valuation._
import starling.titan._
import starling.utils.{Log, Stopwatch}
import starling.instrument.physical.PhysicalMetalForward
import valuation.QuotaValuation
import com.trafigura.services.TitanSnapshotIdentifier
import starling.daterange.Day
import starling.titan.EDMConversions._
import starling.db.SnapshotID

/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider,
  titanTradeStore : TitanTradeStore
)
  extends ValuationServiceApi with Log {

  /**
   * Uses a valuation snapshot if one occurred today, otherwise the most recent snapshot
   * before today. There seeming little reason to use an old valuation snapshot - which only tells us
   * it could value all trades at that time.
   *
   * In the former case we use today as the observation day, otherwise
   * we use the previous weekday, regardless of how much earlier the snapshot day might have been, hence
   * it is possible that the environment corresponding to that snapshot (and snapshot day) may have
   * its market day moved forward.
   */
  private def bestValuationIdentifier() : TitanMarketDataIdentifier = {
    val today = Day.today
    val (snapshotID, observationDay) : (SnapshotID, Day) = environmentProvider.metalsValuationSnapshots(Some(today)).headOption match {
      case Some(s : SnapshotID) if (s.snapshotDay >= today) => (s, today)
      case _ => {
        val snapshotID = environmentProvider.metalsSnapshots(None).filterNot(_.snapshotDay >= today).headOption.getOrElse(throw new Exception("No metals snapshots found"))
        (snapshotID, today.previousWeekday)
      }
    }
    TitanMarketDataIdentifier(TitanSnapshotIdentifier(snapshotID.identifier), observationDay)
  }
  
  def latestSnapshotID() : TitanSnapshotIdentifier = environmentProvider.latestMetalsValuationSnapshot.toSerializable

  def valueAllTradeQuotas(maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier] = None) : (TitanMarketDataIdentifier, Map[String, Either[String, List[QuotaValuation]]]) = {

    val marketDataIdentifier = maybeMarketDataIdentifier.getOrElse(bestValuationIdentifier())
    log.info("valueAllTradeQuotas called with market identifier %s".format(marketDataIdentifier))
    val sw = new Stopwatch()
    val forwards : List[PhysicalMetalForward] = titanTradeStore.getAllForwards().collect{case (_, Right(fwd)) => fwd}.toList
    log.info("Got Edm Trade results, trade result count = " + forwards.size)
    val env = environmentProvider.environment(marketDataIdentifier)
    val valuations = forwards.map{
      fwd =>
        (fwd.titanTradeID, fwd.costsAndIncomeQuotaValueBreakdown(env))
    }.toMap

    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.values.partition(_ isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    (marketDataIdentifier, valuations)
  }

  def valueSingleTradeQuotas(tradeID : String, maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier]) : (TitanMarketDataIdentifier, Either[String, List[QuotaValuation]]) = {

    val marketDataIdentifier = maybeMarketDataIdentifier.getOrElse(bestValuationIdentifier())
    log.info("valueSingleTradeQuotas called for %s with market data identifier %s".format(tradeID, marketDataIdentifier))
    val env = environmentProvider.environment(marketDataIdentifier)
    (marketDataIdentifier, valueSingleTradeQuotas(tradeID, env))
  }

  def valueSingleTradeQuotas(tradeId : String, env : Environment): Either[String, List[QuotaValuation]] = {
    titanTradeStore.getForward(tradeId) match {
      case Right(forward) => forward.costsAndIncomeQuotaValueBreakdown(env)
      case Left(err) => Left(err)
    }
  }

  /**
   * Return all snapshots that are valid for a given observation day, ordered from most recent to oldest
   */
  def marketDataValuationSnapshotIDs(observationDay: Option[Day] = None): List[TitanSnapshotIdentifier] = {
    environmentProvider.metalsValuationSnapshots(observationDay).sortWith(_ > _).map{id => TitanSnapshotIdentifier(id.identifier)}
  }

}

