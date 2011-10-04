package starling.db

import starling.daterange.{Day, Timestamp}
import starling.gui.api.{SnapshotIDLabel, MarketDataSelection}

case class SnapshotID(
        observationDay: Day,
        id: Int,
        timestamp : Timestamp,
        marketDataSelection : MarketDataSelection,
        version:Int
        ) extends Ordered[SnapshotID] {

  def compare(rhs: SnapshotID) = {
    if (observationDay == rhs.observationDay) {
      id - rhs.id
    } else {
      observationDay.compareTo(rhs.observationDay)
    }
  }
  def shortString = observationDay + " (s" + id + ")"

  def label = SnapshotIDLabel(observationDay, id, timestamp, version)

  /**
   * Used to identify a unique snapshot to the outside world. Not exposing 'id' itself in case
   * we ever want to change our internal identifiers
   */
  def identifier = id.toString
}

object SnapshotID {
  def apply(rs: ResultSetRow): SnapshotID = SnapshotID(rs.getDay("observationDay"), rs.getInt("snapshotID"),
    rs.getTimestamp("snapshotTime"), rs.getObject[MarketDataSelection]("marketDataSelection"), rs.getInt("version"))
}
