package starling.tradeimport

import starling.systemofrecord.SystemOfRecord
import starling.instrument._
import java.util.concurrent._
import atomic.AtomicBoolean
import locks.ReentrantLock
import starling.gui.api.TradeImportResult
import starling.daterange.{Timestamp, DayAndTime, Day}
import starling.utils.{NamedThreadFactory, Log, Stopwatch}
import starling.db._
import starling.tradestore.{TradeStore}
import starling.trade.{Trade, TradeID, TradeSystem, TradeAttributes}
import starling.utils.sql.Clause
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

class TradeImporter(systemOfRecord: SystemOfRecord, tradeStore: TradeStore,
                    writeTimestamp: Timestamp = new Timestamp) {

  def importAll(allTrades: Option[Seq[Trade]] = None): Boolean = TradeImporter.lock.synchronized {
    Log.infoWithTime("Running trade import for " + systemOfRecord) {
      val sw = new Stopwatch

      val trades = allTrades match {
        case Some(t) => t
        case None => {
          val temp = ArrayBuffer[Trade]()
          systemOfRecord.allTrades {
            t => {
              temp += t
            }
          }
          temp
        }
      }
      tradeStore.storeTrades( (trade) => true, trades, writeTimestamp)._2
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



