package starling.tradestore

import starling.pivot._

case class TradePredicate(filter:List[(Field,Selection)], selection:List[List[(Field,Selection)]] = List()) {
  override def toString = {
    filter.map{p=>p._1.name + " = " + (p._2 match {case SomeSelection(values)=>values.mkString(", ")})}.mkString(" ") + " " +
            selection.map(cell=> cell.map(p=>p._1.name + " = " + (p._2 match {case SomeSelection(values)=>values.mkString(", ")}))).mkString(" or ")
  }

  def addFilter(field: Field, value: Set[Any]): TradePredicate =
    copy(filter = (field, new SomeSelection(value)) :: filter)
}

object TradePredicate {
  val Null = TradePredicate(List(), List())
}