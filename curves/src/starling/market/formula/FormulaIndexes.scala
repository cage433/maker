package starling.market
package formula

class FormulaIndexException(s: String) extends Exception(s) 

trait FormulaIndexes {
  def eaiQuoteMap: Map[Int, FormulaIndex]

  def indexes = eaiQuoteMap.map(_._2).toList

  def index(quoteID: Int) = eaiQuoteMap.get(quoteID) match {
    case Some(fi) => fi
    case None => throw new Exception("Not a formula index: " + quoteID)
  }

  def isFormulaIndex(quoteID: Int) = indexes.contains(quoteID)
}