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

/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider,
  titanTradeStore : TitanTradeStore
)
  extends ValuationServiceApi with Log {

  def latestSnapshotID() : TitanSnapshotIdentifier = environmentProvider.latestMetalsValuationSnapshot.toSerializable

  def valueAllTradeQuotas(marketDataIdentifier : TitanMarketDataIdentifier) : Map[String, Either[String, List[QuotaValuation]]] = {

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
    valuations
  }

  def valueSingleTradeQuotas(tradeID : String, marketDataIdentifier : TitanMarketDataIdentifier) : Either[String, List[QuotaValuation]] = {

    log.info("valueSingleTradeQuotas called for %s with market data identifier %s".format(tradeID, marketDataIdentifier))
    val env = environmentProvider.environment(marketDataIdentifier)
    valueSingleTradeQuotas(tradeID, env)
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
  def marketDataSnapshotIDs(observationDay: Option[Day] = None): List[TitanSnapshotIdentifier] = {
    environmentProvider.metalsValuationSnapshots(observationDay).sortWith(_ > _).map{id => TitanSnapshotIdentifier(id.identifier)}
  }

}

