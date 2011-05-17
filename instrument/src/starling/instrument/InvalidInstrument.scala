package starling.instrument


import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.DayAndTime
import starling.curves.{Environment}
import starling.quantity.{UOM, Quantity}
import starling.daterange.Day

class InvalidInstrumentException extends RuntimeException


/**
 *  Trades that cannot be read from the DB will become one of these
 */
trait InvalidInstrument extends Tradeable {
  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map())

  def deltaStepType() = throw new UnsupportedOperationException
}

case class ErrorInstrument(exception : String) extends InvalidInstrument with UTP {
  def isLive(dayAndTime: DayAndTime) = true
  def tradeableType = ErrorInstrument
  def tradeableDetails = Map[String, Any]("error" -> exception)
  override def hashCode = exception.lines.next.hashCode

  override def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(this->1))

  override def equals(that: Any) = that match {
    case other : ErrorInstrument => exception == other.exception
    case _ => false
  }

  def details = tradeableDetails
  def instrumentType = ErrorInstrument


  def valuationCCY = throw new Exception("Error Instrument")
  def assets(env: Environment) = throw new Exception("Error Instrument")

  def volume = Quantity.NULL

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = Nil

  def * (scale : Double) = this
  def periodKey = None

  def price(env : Environment) = throw new Exception("Error Instrument")
}

object ErrorInstrument extends TradeableType[ErrorInstrument] with InstrumentType[ErrorInstrument] {
  val id = -1
  val name = "Error Instrument"
  def createTradeable(row: RichInstrumentResultSetRow) = ErrorInstrument(row.getString("error").trim)
  def sample = {
    ErrorInstrument("sample")
  }
}

case class NullInstrument() extends InvalidInstrument {
  def isLive(dayAndTime: DayAndTime) = throw new Exception("Null instrument")
  def tradeableType = NullInstrument
  def tradeableDetails = throw new Exception("Null instrument")
}

object NullInstrument extends TradeableType[NullInstrument] {
  val name = "Null Instrument"
  def createTradeable(row: RichInstrumentResultSetRow) = throw new Exception("Null instrument")
  def sample = {
    NullInstrument()
  }
}
