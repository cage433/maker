package starling.db

import starling.daterange.{Day, Timestamp}
import starling.gui.api.{SnapshotType, SnapshotIDLabel, MarketDataSelection}

case class SnapshotID(
        id: Int,
        timestamp : Timestamp,
        marketDataSelection : MarketDataSelection,
        snapshotType : SnapshotType,
        version:Int
        ) extends Ordered[SnapshotID] {

  def compare(rhs: SnapshotID) = { id - rhs.id }

  def snapshotDay = timestamp.day

  def shortString = timestamp + "(s" + id + ")"

  def label = SnapshotIDLabel(id, timestamp, marketDataSelection, snapshotType, version)

  /**
   * Used to identify a unique snapshot to the outside world. Not exposing 'id' itself in case
   * we ever want to change our internal identifiers
   */
  def identifier = id.toString
}

object SnapshotID {
  def apply(rs: ResultSetRow): SnapshotID = SnapshotID(
    rs.getInt("snapshotID"),
    rs.getTimestamp("snapshotTime"),
    rs.getObject[MarketDataSelection]("marketDataSelection"),
    SnapshotType(rs.getString("snapshotType")),
    rs.getInt("commitid")
  )
}
