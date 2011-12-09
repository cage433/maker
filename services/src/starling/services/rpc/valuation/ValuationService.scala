package starling.services.rpc.valuation

import starling.curves.Environment
import com.trafigura.services.valuation._
import starling.titan._
import starling.utils.{Log, Stopwatch}
import starling.instrument.physical.PhysicalMetalForward
import valuation.QuotaValuation
import com.trafigura.services.TitanSnapshotIdentifier
import starling.titan.EDMConversions._
import starling.db.SnapshotID
import starling.daterange.{TimeOfDay, Day}

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
   * In either case the market day is today
   */
  def bestValuationIdentifier() = {
    val today = Day.today
    val latestMetalsValuationSnapshot = environmentProvider.latestMetalsValuationSnapshot
    var latestValuationObservationDay = latestMetalsValuationSnapshot.observationDay.get
    if (latestValuationObservationDay < today.addWeekdays(-3)){
      val snapshotID = environmentProvider.metalsSnapshots(None).filter(_.snapshotDay >= today).headOption.getOrElse(throw new Exception("No metals snapshots found"))
      (snapshotID, today.previousWeekday, today)
    } else {
      (latestMetalsValuationSnapshot, latestValuationObservationDay, today)
    }
  }
  
  def latestSnapshotID() : TitanSnapshotIdentifier = environmentProvider.latestMetalsValuationSnapshot.toSerializable

  private def marketDataIdentifierAndEnvironment(maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier]) : (TitanMarketDataIdentifier, Environment) = {
    def makeEnv(snapshotID : SnapshotID, observationDay : Day, marketDay : Day) = environmentProvider.valuationServiceEnvironment(snapshotID, observationDay, marketDay)
    maybeMarketDataIdentifier match {
      case Some(tmdi @ TitanMarketDataIdentifier(TitanSnapshotIdentifier(snapshotIdentifier), observationDay, marketDay)) => {
        val snapshotID = environmentProvider.snapshotIDFromIdentifier(snapshotIdentifier)
        (tmdi, makeEnv(snapshotID, observationDay, marketDay))
      }
      case None => {
        val (snapshotID, observationDay, marketDay) = bestValuationIdentifier()
        (TitanMarketDataIdentifier(TitanSnapshotIdentifier(snapshotID.identifier), observationDay, marketDay), makeEnv(snapshotID, observationDay, marketDay))
      }
    }
  }
  def valueAllTradeQuotas(maybeMarketDataIdentifier : Option[TitanMarketDataIdentifier] = None) : (TitanMarketDataIdentifier, Map[String, Either[String, List[QuotaValuation]]]) = {

    val (marketDataIdentifier, env) = marketDataIdentifierAndEnvironment(maybeMarketDataIdentifier)
    log.info("valueAllTradeQuotas called with market identifier %s".format(marketDataIdentifier))
    val sw = new Stopwatch()
    val forwards : List[PhysicalMetalForward] = titanTradeStore.getAllForwards().collect{case (_, Right(fwd)) => fwd}.toList
    log.info("Got Edm Trade results, trade result count = " + forwards.size)
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

  /**
   * Return all snapshots that are valid for a given observation day, ordered from most recent to oldest
   */
  def marketDataValuationSnapshotIDs(observationDay: Option[Day] = None): List[TitanSnapshotIdentifier] = {
    environmentProvider.metalsValuationSnapshots(observationDay).sortWith(_ > _).map{id => TitanSnapshotIdentifier(id.identifier)}
  }

}

object ValuationService{
  def buildEnvironment(
    provider : EnvironmentProvider,
    snapshotID : SnapshotID,
    observationDay : Day,
    marketDay : Day
  ) : Environment = {
    provider.environment(snapshotID, observationDay).undiscounted.forwardState(marketDay.endOfDay)
  }
}

