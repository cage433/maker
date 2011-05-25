package starling.market.rules

case class Precision(default: Int, clearPort: Int)

trait PrecisionRules {
  def rule(eaiQuoteID: Int): Option[Precision]
}