package starling.metals

import swing.event.Event
import com.trafigura.shared.events.{RemovedEventVerb, CancelledEventVerb, UpdatedEventVerb, CreatedEventVerb}
import starling.gui.api.PricingGroupMarketDataUpdate._
import starling.services.rpc.valuation.EnvironmentProvider
import starling.daterange.Day
import starling.gui.api.SnapshotMarketDataVersion._
import starling.curves.{Environment, MissingMarketDataException}
import starling.quantity.Quantity
import starling.titan.valuation.QuotaValuation
import starling.titan.{TitanTradeStore, TitanTradeStoreManager, TitanTradeUpdateResult}
import collection.immutable.Map
import starling.utils.{Log}
import starling.db.SnapshotID
import starling.gui.api._
import starling.utils.ImplicitConversions._
import starling.manager.{Broadcaster, Receiver}
import starling.instrument.physical.PhysicalMetalForward

/**
 * Creates a snapshot when all metals trades can be valued without a market data exception
 */
class MetalValuationSnapshotCreator( broadcaster:Broadcaster,
                           environmentProvider:EnvironmentProvider,
                           titanTradeStore:TitanTradeStore) extends Receiver with Log {

  import MetalValuationSnapshotCreator._

  def event(event: Event) = {
    event partialMatch {
      case PricingGroupMarketDataUpdate(PricingGroup.Metals, version, _, _) => doCheck(version)
    }
  }

  private def doCheck(version: Int) {
    val versionForLatestSnapshot = SnapshotMarketDataVersion(environmentProvider.latestMetalsValuationSnapshot.label)
    val currentVersion = SpecificMarketDataVersion(version)

    val observationDay = Day.today
    val previousEnv = environmentProvider.valuationServiceEnvironment(versionForLatestSnapshot, observationDay, observationDay)
    val newEnv = environmentProvider.valuationServiceEnvironment(currentVersion, observationDay, observationDay)
    var forwards: Map[String, Either[String, PhysicalMetalForward]] = titanTradeStore.getAllForwards

    valuationChange(previousEnv, newEnv, forwards) match {
      case OldAndNewValuationsNotAvailable |
           OldValuationsAvailableButNewUnavailable |
           ValuationUnchanged =>
      case FirstNewValuation => {
        log.info("Snapshoting because first Metals valuation succeeded for " + observationDay)
        environmentProvider.makeValuationSnapshot(version)
      }
      case SecondOrLaterSuccessfulValuation(numChangedTrades) => {
        log.info("Snapshoting because Metals valuation changed on " + observationDay + ", changed " + numChangedTrades + " + trade(s)")
        environmentProvider.makeValuationSnapshot(version)
      }
    }
  }
}

/**
 * Extract as much functionality here to make unit testing possible without
 * having to implement Broadcasters, TitanTradeStores etc
 */
object MetalValuationSnapshotCreator{

  trait ValuationResult
  object MissingMarketDataResults extends ValuationResult
  case class SuccessResults(values:Map[String,List[QuotaValuation]]) extends ValuationResult

  private def valueAll(env:Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]):ValuationResult = {
    val valuations: Map[String, Option[List[QuotaValuation]]] = forwards.mapValues { value =>
      value match {
        case Right(fwd) => {
          fwd.costsAndIncomeQuotaValueBreakdown(env) match {
            case Left(someErrorThatIsntMissingMarketData) => Some(Nil)
            case Right(quotaValues) if quotaValues.exists(_.hasMissingMarketDataError) => None
            case Right(quotaValues) => Some(quotaValues)
          }
        }
        case _ => Some(Nil)
      }
    }
    if (valuations.values.find(_ == None).isDefined) {
      MissingMarketDataResults
    } else {
      SuccessResults(valuations.mapValues(_.get))
    }
  }

  def valueAll(previousEnv:Environment, newEnv : Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]): (ValuationResult, ValuationResult) = (
    valueAll(previousEnv, forwards),
    valueAll(newEnv, forwards)
  )

  trait ValuationChangeResult
  object OldAndNewValuationsNotAvailable extends ValuationChangeResult
  object OldValuationsAvailableButNewUnavailable extends ValuationChangeResult
  object FirstNewValuation extends ValuationChangeResult
  object ValuationUnchanged extends ValuationChangeResult
  case class SecondOrLaterSuccessfulValuation(numTradeChanged : Int) extends ValuationChangeResult

  def valuationChange(previousEnv:Environment, newEnv : Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]) : ValuationChangeResult =
  {
    val (previousValues, newValues) = valueAll(previousEnv, newEnv, forwards)
    (previousValues, newValues) match {
      case (MissingMarketDataResults, MissingMarketDataResults) => OldAndNewValuationsNotAvailable
      case (MissingMarketDataResults, SuccessResults(_)) => FirstNewValuation
      case (SuccessResults(_), MissingMarketDataResults) => OldValuationsAvailableButNewUnavailable
      case (SuccessResults(v1), SuccessResults(v2)) => {
        val changedTrades = v1.filter {
          case (tradeID, quotaValues) => quotaValues != v2(tradeID)
        }
        if (changedTrades.isEmpty)
          ValuationUnchanged
        else
          SecondOrLaterSuccessfulValuation(changedTrades.size)
      }
    }
  }
}