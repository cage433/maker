package starling.services.rpc.valuation

import starling.curves.Environment
import com.trafigura.services.valuation._
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import starling.services.rabbit._
import starling.titan._
import com.trafigura.edm.shared.types.TitanId
import starling.utils.{Log, Stopwatch}
import starling.rmi.RabbitEventDatabase
import starling.daterange.Day
import starling.instrument.physical.PhysicalMetalForward
import valuation.QuotaValuation
import starling.db.SnapshotID
import com.trafigura.services.TitanSnapshotIdentifier


/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider,
  titanTradeStore : TitanTradeStore
)
  extends ValuationServiceApi with Log {

  def valueAllTradeQuotas(marketDataIdentifier : TitanMarketDataIdentifier) : Map[String, Either[String, List[QuotaValuation]]] = {

    log.info("valueAllTradeQuotas called with market identifier %s".format(marketDataIdentifier))
    val sw = new Stopwatch()
    val forwards : List[PhysicalMetalForward] = titanTradeStore.getAllForwards()
    log.info("Got Edm Trade results, trade result count = " + forwards.size)
    val env = environmentProvider.environment(marketDataIdentifier)
    val valuations = forwards.map{
      fwd =>
        (fwd.tradeID, fwd.costsAndIncomeQuotaValueBreakdown(env))
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
    val forward : PhysicalMetalForward = titanTradeStore.getForward(tradeId)
    forward.costsAndIncomeQuotaValueBreakdown(env)
  }


  /**
   * Return all snapshots that are valid for a given observation day, ordered from most recent to oldest
   */
  def marketDataSnapshotIDs(observationDay: Option[Day] = None): List[TitanSnapshotIdentifier] = {
    environmentProvider.snapshots().filter {
      snapshotID =>

        observationDay match {
          case Some(day) => day <= snapshotID.observationDay
          case None => true
        }
    }.sortWith(_ > _).map{id => TitanSnapshotIdentifier(id.identifier)}
  }

}

