package starling.tradeimport

import starling.systemofrecord.SystemOfRecord
import starling.instrument._
import java.util.concurrent._
import atomic.AtomicBoolean
import locks.ReentrantLock
import starling.gui.api.TradeImportResult
import starling.daterange.{Timestamp, DayAndTime, Day}
import starling.manager.NamedThreadFactory
import starling.utils.{Log, Stopwatch}
import starling.db._
import starling.tradestore.{TradeStore}
import starling.instrument.Trade
import starling.instrument.{TradeID, TradeSystem, TradeAttributes}
import starling.dbx.Clause
import collection.mutable.{ArrayBuffer, HashSet => MHashSet}

class TradeImporterFactory(
                            refinedAssignmentImporter: TradeImporter,
                            refinedFixationImporter: TradeImporter
                            ) {
  def apply(tradeSystem: TradeSystem) = {
    tradeSystem match {
      case RefinedAssignmentTradeSystem => refinedAssignmentImporter
      case RefinedFixationTradeSystem => refinedFixationImporter
    }
  }
}

class TradeImporter(val tradeSystem:TradeSystem, systemOfRecord: SystemOfRecord, tradeStore: TradeStore) extends Log {

  def importAll(allTrades: Option[Seq[Trade]] = None, writeTimestamp: Timestamp = new Timestamp()): Boolean = TradeImporter.lock.synchronized {
    log.infoWithTime("Running trade import for " + systemOfRecord + " with timestamp " + writeTimestamp) {
      val trades = allTrades match {
        case Some(t) => t
        case None => systemOfRecord.allTrades
      }
      assert(trades.map(_.tradeID).toSet.size == trades.size, "Duplicate trade ids!")
      tradeStore.storeTrades( (trade) => true, trades, writeTimestamp).changed
    }
  }
}

object TradeImporter {
  /**
   * We can't have more than one import happening at a time. All importers
   * share tables during an import so we have to have a global lock
   */
  val lock = new Object


}



