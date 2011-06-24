package starling.market

import starling.utils.StarlingEnum

/**
 * Nicked from Kudu. The level of some indexes observed price
 */
case class Level(name: String) extends Ordered[Level] {
  def compare(that: Level) = this.name.compare(that.name)
}

object Level extends StarlingEnum(classOf[Level], (l: Level) => l.name) {
  val Live = Level("Live")
  val Close = Level("Close")
  val IndexLevel = Level("Index")
  val High = Level("High")
  val Mid = Level("Mid")
  val MidPoint = Level("MidPoint")
  val Low = Level("Low")
  val Val = Level("Val")
  val Bid = Level("Bid")
  val Ask = Level("Ask")
  val Spot = Level("Spot")
  val Settle = Level("Settle")
  val Unknown = Level("Unknown")
}

