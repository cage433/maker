package starling.instrument


import starling.curves.Environment
import java.sql.ResultSet
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.{Day, DayAndTime}
import starling.quantity.{NamedQuantity, UOM}

case class DeletedInstrument() extends Tradeable {
  def isLive(dayAndTime: DayAndTime) = throw new IllegalStateException("Deleted instrument")
  def persistedTradeableDetails = Map.empty
  def tradeableType = DeletedInstrument
  def asUtpPortfolio(tradeDay:Day) = throw new IllegalStateException("Can't have UTP for a deleted instrument")
  def explanation(env : Environment) : NamedQuantity = throw new IllegalStateException("Can't have explanation for a deleted instrument")

  def deltaStepType() = throw new UnsupportedOperationException()
  def valuationCCY = throw new UnsupportedOperationException()
}

object DeletedInstrument extends TradeableType[DeletedInstrument] {
  val name = "DeletedInstrument"
  def createTradeable(row: RichInstrumentResultSetRow) = DeletedInstrument()
  def sample = {
    DeletedInstrument()
  }
}
