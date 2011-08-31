package starling.instrument

import starling.curves._
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange._
import starling.market.{Index, FuturesMarket}
import starling.quantity.{SimpleNamedQuantity, Quantity, UOM}

/**
 * Used to represent the cash part of a Futures's UTP
 * The EnvironmentDifferentiable is used to split pivot 
 * report values for this UTP across the associated Future's risk rows
 */
case class BankAccount(volume : Quantity, market : Option[FuturesMarket], index : Option[Index], delivery : Period) extends UTP {
  def detailsForUTPNOTUSED = Map[String, Any]("Amount" -> volume, "Period" -> delivery) ++ market.map("Market" -> _).toMap ++ index.map("Market" -> _).toMap
  def instrumentType = BankAccount
  def isLive(dayAndTime: DayAndTime) = true
  def valuationCCY = volume.uom
  def assets(env: Environment) = Assets(Asset.knownCash(env.marketDay.day, volume, env)) //Fix me?

  def explanation(env: Environment) = new SimpleNamedQuantity("Amount", assets(env).mtm(env, UOM.USD))

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(marketDay.day)
  def * (scale : Double) = copy(volume = volume * scale)
  def periodKey = None
  def price(env : Environment) = Quantity.NULL
}

object BankAccount extends InstrumentType[BankAccount] {
  val name = "Bank Account"
}

