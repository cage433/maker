package starling.tradestore

import starling.pivot._

case class TradePredicate(filter:List[(Field,Selection)], selection:List[List[(Field,Selection)]] = List()) {
  def addFilter(field: Field, value: Set[Any]): TradePredicate =
    copy(filter = (field, new SomeSelection(value)) :: filter)
}

object TradePredicate {
  val Null = TradePredicate(List(), List())
}