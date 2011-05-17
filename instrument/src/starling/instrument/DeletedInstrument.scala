package starling.instrument


import starling.curves.Environment
import java.sql.ResultSet
import starling.quantity.UOM
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.{Day, DayAndTime}

case class DeletedInstrument() extends Tradeable {
  def isLive(dayAndTime: DayAndTime) = throw new IllegalStateException("Deleted instrument")
  def tradeableDetails = Map.empty
  def tradeableType = DeletedInstrument
  def asUtpPortfolio(tradeDay:Day) = throw new IllegalStateException("Can't have UTP for a deleted instrument")

  def deltaStepType() = throw new UnsupportedOperationException()
}

object DeletedInstrument extends TradeableType[DeletedInstrument] {
  val name = "DeletedInstrument"
  def createTradeable(row: RichInstrumentResultSetRow) = DeletedInstrument()
  def sample = {
    DeletedInstrument()
  }
}
