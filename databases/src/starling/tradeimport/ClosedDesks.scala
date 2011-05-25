package starling.tradeimport

import starling.db.DB
import starling.utils.sql.QueryBuilder._
import starling.calendar.BusinessCalendarSet
import starling.daterange.{Day, Timestamp}
import collection.SortedMap
import collection.immutable.TreeMap
import starling.gui.api.{DeskCloseFailed, DeskClosed, TradeTimestamp, Desk}
import starling.utils.{Log, Broadcaster, StackTraceToString}

class ClosedDesks(broadcaster: Broadcaster, db: DB) {
  def lastClosed: Map[Desk, Timestamp] = {
    db.queryWithResult((
            select("desk, max(tradeTimestamp) as cl")
                    from ("ClosedDesks")
                    groupBy ("desk")
            )) {
      rs => {
        (Desk(rs.getString("desk")), rs.getTimestamp("cl"))
      }
    }.toMap
  }

  def closeDesk(desk: Desk, closedDay: Day, closedTime: Timestamp = new Timestamp) {
    db.inTransaction(writer => {
      writer.insert("ClosedDesks", Map("desk" -> desk.name, "tradeTimestamp" -> closedTime, "closedDay" -> closedDay))
    })
    try {
      // we don't want the notify failing to cause the desk close to roll back
      val desksByDay = closedDesksByDay(desk)
      val newestDay = desksByDay.keys.toList.sortWith(_ > _).head
      val newestTimestamp = desksByDay(newestDay).sortWith(_.timestamp > _.timestamp).head
      broadcaster.broadcast(DeskClosed(desk, newestTimestamp))
    }
    catch {
      case e => Log.error("Failed to notify about desk close: " + (desk, closedDay, closedTime), e)
    }
  }

  def closeDeskWithError(desk: Desk, closedDay: Day, e: Throwable, closedTime: Timestamp) {
    db.inTransaction(writer => {
      writer.insert("ClosedDesks", Map("desk" -> desk.name, "tradeTimestamp" -> closedTime, "closedDay" -> closedDay, "error" -> StackTraceToString(e)))
    })
    try {
      val desksByDay = closedDesksByDay(desk)
      val newestDay = desksByDay.keys.toList.sortWith(_ > _).head
      val newestTimestamp = desksByDay(newestDay).sortWith(_.timestamp > _.timestamp).head
      broadcaster.broadcast(DeskCloseFailed(desk, newestTimestamp, e))
    }
    catch {
      case e2 => Log.error("Failed to notify about failed desk close: " + (desk, closedDay, closedTime, StackTraceToString(e)), e2)
    }
  }

  private def closedTimes: Map[Desk, List[(Day, Timestamp, Option[String])]] = {
    db.queryWithResult((
            select("desk, closedDay, tradeTimestamp as cl, error")
                    from ("ClosedDesks")
            )) {
      rs => {
        val error = rs.getString("error") match {
          case null => None
          case e => Some(e)
        }
        (Desk(rs.getString("desk")), rs.getDay("closedDay"), rs.getTimestamp("cl"), error)
      }
    }.groupBy(_._1).mapValues(_.map(a => (a._2, a._3, a._4)))
  }

  def closedDesksByDay: Map[Desk, Map[Day, List[TradeTimestamp]]] = {
    Map() ++ closedTimes.mapValues {
      timestamps => {
        val mapOfDayToDeskCloses = timestamps.groupBy(_._1)
        Map() ++ mapOfDayToDeskCloses.map {
          case (day, closes) => day -> closes.sortWith(_._2 < _._2).zipWithIndex.map {
            case ((day, timestamp, error), num) => TradeTimestamp(timestamp, day, (num + 1), error)
          }
        }
      }
    }
  }

  def closedDesksByOffset(desk: Desk, closeDay: Day): TradeTimestamp = {
    val booksByDay = closedDesksByDay(desk)
    val sortedDays = booksByDay.keys.toList.sorted
    val dayToUse = sortedDays.span(_ <= closeDay)._1.last
    booksByDay(dayToUse).last
  }
}