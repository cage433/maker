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
class MetalValuationSnapshotCreator(broadcaster:Broadcaster,
                           environmentProvider:EnvironmentProvider,
                           titanTradeStore:TitanTradeStore) extends Receiver with Log {

  import MetalValuationSnapshotCreator._

  def event(event: Event) = {
    event partialMatch {
      case PricingGroupMarketDataUpdate(PricingGroup.Metals, version, _, _) => doCheck(version)
    }
  }

  private def doCheck(version: Int) {
    var forwards: Map[String, Either[String, PhysicalMetalForward]] = titanTradeStore.getAllForwards
    if (!forwards.isEmpty) /*Otherwise empty environments have valuation snapshots regardless of whether they have any market data */{
      val (_, previousEnv) = environmentProvider.lastValuationSnapshotEnvironment

      val today = Day.today
      val currentVersion = SpecificMarketDataVersion(version)
      val newEnv = environmentProvider.valuationServiceEnvironment(currentVersion, today.endOfDay, today)

      valuationChange(previousEnv, newEnv, forwards) match {
        case ValuationChanged(changedIDs) => {
          log.info("Snapshotting because first metals valuation changed for " + changedIDs.size + "trades ")
          val snapshotID = environmentProvider.makeValuationSnapshot(version)
          broadcaster.broadcast(RefinedMetalsManyValuationsChanged(snapshotID.label))
        }
        case ValuationUnchanged => {
          log.info("Not snapshotting because  no valuations changed")
        }
      }
    }
  }
}

/**
 * Extract as much functionality here to make unit testing possible without
 * having to implement Broadcasters, TitanTradeStores etc
 */
object MetalValuationSnapshotCreator{


  private type ValuationsResult = Map[String,Either[String, List[QuotaValuation]]]
  private def valueAll(env:Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]):ValuationsResult = {
    forwards.mapValues { value =>
      value match {
        case Right(fwd) => {
          fwd.costsAndIncomeQuotaValueBreakdown(env)
        }
        case Left(error) => Left(error)
      }
    }
  }

  def valueAll(previousEnv:Environment, newEnv : Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]): (ValuationsResult, ValuationsResult) = (
    valueAll(previousEnv, forwards),
    valueAll(newEnv, forwards)
  )

  trait ValuationChangeResult
  case class ValuationChanged(changedTradeIDs : Set[String]) extends ValuationChangeResult
  object ValuationUnchanged extends ValuationChangeResult

  def valuationChange(previousEnv:Environment, newEnv : Environment, forwards: Map[String, Either[String, PhysicalMetalForward]]) : ValuationChangeResult =
  {
    val (previousValues, newValues) = valueAll(previousEnv, newEnv, forwards)
    val changedIDs = forwards.keySet.filterNot{id => previousValues(id) == newValues(id)}
    if (changedIDs.isEmpty)
      ValuationUnchanged
    else
      ValuationChanged(changedIDs)
  }
}
