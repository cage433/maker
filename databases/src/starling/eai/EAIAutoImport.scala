package starling.eai

import instrumentreaders.EAISystemOfRecord
import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.gui.api.Desk
import java.util.{Timer, TimerTask}
import starling.systemofrecord.SystemOfRecord
import starling.tradestore.TradeStore
import starling.richdb.RichDB
import starling.daterange.{Day, Timestamp}
import starling.tradestore.eai.EAITradeStore
import starling.tradeimport.{TradeImporter, ClosedDesks}
import starling.calendar.BusinessCalendarSet
import collection.mutable.ArrayBuffer
import starling.trade.Trade
import org.springframework.dao.DeadlockLoserDataAccessException
import starling.utils.{RepeatingTask, Log}


/**
 * @param runEvery number of seconds to run after, repeats
 */
class EAIAutoImport(runEvery: Int, starlingDB: RichDB, externalDB: RichDB, eaiStrategyDB: EAIStrategyDB, tradeStores: Map[Book,TradeStore], closedDesks: ClosedDesks, enabledDesks: Set[Desk])
  extends RepeatingTask(runEvery, "EAIAutoImport") {

  import EAISystemOfRecord._

  val bookMap = Map(LONDON_DERIVS_BOOK -> Desk.LondonDerivatives,
    LONDON_DERIVS_OPTIONS_BOOK -> Desk.LondonDerivativesOptions,
    CrudeSpecNorthSea -> Desk.CrudeSpecNorthSea,
    GasolineSpec -> Desk.GasolineSpec,
    HoustonDerivatives -> Desk.HoustonDerivatives)
    .filter(e => enabledDesks.contains(e._2))

  // filter only import desks that are enabled in props

  case class Closed(bookID: Int, downloadID: Int, when: Timestamp, error: Option[String])

  def task {
    try {
      runTask
    } catch {
      case e => {
        // the task can sometimes throw exceptions when the db is in a bad state (shutting down, off and being patched etc.)
        // so just ignore the exception, the task will run again later.
        Log.warn("Failed with eaiautoimport, will try again later.", e)
      }
    }
  }
  private def runTask {

    // book closes is an order list (newest last) of the downloads (bookid, downloadid, timestamp)
    val bookCloses: List[Closed] = (externalDB.queryWithResult((
      select("BookID, id, EndTimestamp, ErrorTimestamp, Error")
        from ("tblDownloads")
        where (
        (("EndTimestamp" isNotNull) or ("ErrorTimestamp" isNotNull)) and
          ("bookID" in bookMap.keys)
        )
      //        orderBy ("EndTimestamp" asc)
      )) {
      rs => {
        val error = rs.getString("Error")
        error match {
          case null => {
            Closed(rs.getInt("BookID"), rs.getInt("id"), rs.getTimestamp("EndTimestamp"), None)
          }
          case _ => {
            Closed(rs.getInt("BookID"), rs.getInt("id"), rs.getTimestamp("ErrorTimestamp"), Some(error))
          }
        }

      }
    }).sortWith(_.when < _.when)

    val lastClosedForUs = closedDesks.lastClosed

    // book closes we haven't seen yet
    val newerBookCloses = bookCloses.filter {
      case Closed(book, downloadID, timestamp, _) => {
        val desk = bookMap(book)
        lastClosedForUs.get(desk) match {
          case Some(ts) => timestamp > ts
          case None => true
        }
      }
    }

    if (newerBookCloses.nonEmpty) {
      eaiStrategyDB.refresh
      Log.info("We need to import " + newerBookCloses.size + " book closes for EAI")

      val backOffMs = 60 * 1000
      // this is crazy but we need to wait for a small amount of time so that we don't jump on top of the
      // finishing backup sql running on EAIStarling. The stored procedure could be finished but the index
      // could still be being created.
      Thread.sleep(backOffMs)

      newerBookCloses.map {
        case Closed(bookToImport, downloadID, bookToImportTimestamp, None) => {
          val deskToClose: Desk = bookMap(bookToImport)
          val closeDay: Day = bookToImportTimestamp.day.previousWeekday
          try {
            val maxRetries = 2
            var done = false
            var retryCount = 0

            var trades: ArrayBuffer[Trade] = null
            val systemOfRecord: EAISystemOfRecord = new EAISystemOfRecord(externalDB, bookToImport, downloadID)

            // try to read in all the trades. if we fail with a DeadlockLoserDataAccessException then we backoff
            // and retry. I've no idea why these exceptions happen nor do do the DBAs.
            while (!done) {
              trades = ArrayBuffer[Trade]()
              try {
                systemOfRecord.allTrades {
                  t => {
                    trades += t
                  }
                }
                done = true
              } catch {
                case e: DeadlockLoserDataAccessException if retryCount < maxRetries => {
                  retryCount += 1
                  Thread.sleep(backOffMs)
                }
                case e => throw e
              }
            }

            assert(done) // sanity check

            val tradeStore = tradeStores(Book(bookToImport))
            // we want a transaction around the import and desk close so if the desk close fails we roll back
            // the trade import.
            starlingDB.inTransaction(writer => {
              new TradeImporter(systemOfRecord, tradeStore).importAll(Some(trades), bookToImportTimestamp)
              closedDesks.closeDesk(deskToClose, closeDay, bookToImportTimestamp)
            })
          }
          catch {
            case e => closedDesks.closeDeskWithError(deskToClose, closeDay, e, bookToImportTimestamp)
          }
        }
        case Closed(failedBook, downloadID, timestmap, Some(error)) => {
          val deskToClose: Desk = bookMap(failedBook)
          val closeDay: Day = timestmap.day.previousWeekday
          val e = new Exception(error)
          closedDesks.closeDeskWithError(deskToClose, closeDay, e, timestmap)
        }
      }
    }
  }
}
