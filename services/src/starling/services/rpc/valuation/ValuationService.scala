package starling.services.rpc.valuation

import starling.curves.Environment
import com.trafigura.services.valuation._
import starling.titan._
import starling.utils.{Log, Stopwatch}
import starling.instrument.physical.PhysicalMetalForward
import valuation.QuotaValuation
import com.trafigura.services.TitanSnapshotIdentifier
import starling.db.SnapshotID
import starling.daterange.{DayAndTime, Day}
import starling.gui.api.SnapshotMarketDataVersion

/**
 * Valuation service implementations
 */
case class ValuationService(
  environmentProvider : EnvironmentProvider,
  titanTradeStore : TitanTradeStore
)
  extends ValuationServiceApi with Log {

  /**
   * Use a valuation snapshot if it occurred recently, with the market data from its associated
   * observation day. If no suitable valuation snapshot exists then us the most recent from last night.
   * In either case the market day is today.sortWith(_ > _).map{id => TitanSnapshotIdentifier(id.identifier)}
   */
  private def bestValuationIdentifier() = {
    val today = Day.today
    val snapshotID = environmentProvider.metalsSnapshots(None).headOption.getOrElse(throw new Exception("No metals snapshots found"))
    (snapshotID, today, today)
  }
  
  private def marketDataIdentifierAndEnvironment(maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier]) : (TitanMarketDataIdentifier, Environment) = {
    def makeEnv(snapshotID : SnapshotID, observationDay : DayAndTime, marketDay : Day) = environmentProvider.valuationServiceEnvironment(new SnapshotMarketDataVersion(snapshotID.label), observationDay, marketDay)
    maybeMarketDataIdentifier match {
      case Some(tmdi @ TitanMarketDataIdentifier(TitanSnapshotIdentifier(snapshotIdentifier), observationDay, marketDay)) => {
        val snapshotID = environmentProvider.snapshotIDFromIdentifier(snapshotIdentifier)
        (tmdi, makeEnv(snapshotID, observationDay.endOfDay, marketDay))
      }
      case None => {
        val (snapshotID, observationDay, marketDay) = bestValuationIdentifier()
        (TitanMarketDataIdentifier(TitanSnapshotIdentifier(snapshotID.identifier), observationDay, marketDay), makeEnv(snapshotID, observationDay.endOfDay, marketDay))
      }
    }
  }

  def valueAllTradeQuotas(maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier] = None) : (TitanMarketDataIdentifier, Map[String, Either[String, List[QuotaValuation]]]) = {
    val sw = new Stopwatch()
    val (marketDataIdentifier, env) = marketDataIdentifierAndEnvironment(maybeMarketDataIdentifier)
    log.info("valueAllTradeQuotas called with market identifier %s".format(marketDataIdentifier))
    val forwards : List[PhysicalMetalForward] = titanTradeStore.getAllForwards().collect{case (_, Right(fwd)) => fwd}.toList
    log.info("Got Edm Trade results, trade result count = " + forwards.size)

    val valuations : Map[String, Either[String, List[QuotaValuation]]] = forwards.par.map(
      fwd => fwd.titanTradeID -> fwd.costsAndIncomeQuotaValueBreakdown(env)).seq.toMap

    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.values.partition(_ isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    (marketDataIdentifier, valuations)
  }

  def valueSingleTradeQuotas(tradeID : String, maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier]) : (TitanMarketDataIdentifier, Either[String, List[QuotaValuation]]) = {

    val (marketDataIdentifier, env) = marketDataIdentifierAndEnvironment(maybeMarketDataIdentifier)
    log.info("valueSingleTradeQuotas called for %s with market data identifier %s".format(tradeID, marketDataIdentifier))
    (marketDataIdentifier, valueSingleTradeQuotas(tradeID, env))
  }

  def valueSingleTradeQuotas(tradeId : String, env : Environment): Either[String, List[QuotaValuation]] = {
    titanTradeStore.getForward(tradeId) match {
      case Right(forward) => forward.costsAndIncomeQuotaValueBreakdown(env)
      case Left(err) => Left(err)
    }
  }
}
