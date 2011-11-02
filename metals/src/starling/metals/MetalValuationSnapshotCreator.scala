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
import starling.utils.{Broadcaster, Log, Receiver}
import starling.db.SnapshotID
import starling.gui.api._
import starling.utils.ImplicitConversions._

/**
 * Creates a snapshot when all metals trades can be valued without a market data exception
 */
class MetalValuationSnapshotCreator( broadcaster:Broadcaster,
                           environmentProvider:EnvironmentProvider,
                           titanTradeStore:TitanTradeStore) extends Receiver with Log {
  def event(event: Event) = {
    event partialMatch {
      case PricingGroupMarketDataUpdate(PricingGroup.Metals, version, _, _) => doCheck(version)
    }
  }

  private def doCheck(version:Int) {
    val observationDaysToCheck = (Day.today.addWeekdays(-2) until Day.today).filter(_.isWeekday)
    val latestSnapshot = SnapshotMarketDataVersion(environmentProvider.latestMetalsValuationSnapshot.label)
    val currentVersion = SpecificMarketDataVersion(version)

    observationDaysToCheck.foreach { observationDay => {
      val previousEnv = environmentProvider.environment(latestSnapshot, observationDay)
      val newEnv = environmentProvider.environment(currentVersion, observationDay)
      val previousValues = valueAll(previousEnv)
      val newValues = valueAll(newEnv)

      (previousValues, newValues) match {
        case (MissingMarketDataResults,MissingMarketDataResults) =>
        case (MissingMarketDataResults,SuccessResults(_)) => {
          log.info("Snapshoting because first Metals valuation succeeded for " + observationDay)
          environmentProvider.makeValuationSnapshot(version)
        }
        case (SuccessResults(_),MissingMarketDataResults) =>
        case (SuccessResults(v1),SuccessResults(v2)) => {
          val changedTrades = v1.filter{ case (tradeID, quotaValues) => quotaValues != v2(tradeID) }
          if (!changedTrades.isEmpty) {
            log.info("Snapshoting because Metals valuation changed for " + changedTrades.size + " trades")
            val snapshotID = environmentProvider.makeValuationSnapshot(version)
            broadcaster.broadcast(RefinedMetalsValuationChanged(observationDay, snapshotID.label, changedTrades.keySet))
          }
        }
      }
    } }
  }

  trait ValuationResult
  object MissingMarketDataResults extends ValuationResult
  case class SuccessResults(values:Map[String,List[QuotaValuation]]) extends ValuationResult

  private def valueAll(env:Environment):ValuationResult = {
    val valuations: Map[String, Option[List[QuotaValuation]]] = titanTradeStore.getAllForwards.mapValues { value =>
      value match {
        case Right(fwd) => {
          try {
            val valuations = fwd.quotas.map(_.value(env))
            if (valuations.exists(_.hasError)) None else Some(valuations)
          } catch {
            case e:MissingMarketDataException => None
            case _ => Some(Nil)
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

}