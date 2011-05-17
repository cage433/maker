package starling.models

import scala.math._
import starling.quantity.Quantity

trait CallOrPut{
  def intrinsicPrice(F : Quantity, K : Quantity) : Quantity = {
    assert(F.uom == K.uom)
    Quantity(intrinsic(K.value)(F.value), F.uom)
  }
  def toChar : Char
  def toShortString = toChar.toString
  def payoff(F : Quantity, K : Quantity):Option[Quantity] = {
    val p = intrinsicPrice(F, K)
    if (p.isPositve) Some(p) else None
  }
  def isInTheMoney(K : Double, F : Double) = intrinsic(K)(F) > 0
  def intrinsic(K : Double)(F : Double) : Double = money(K)(F) + stuff(K)(F)
  def money(K : Double)(F : Double) : Double
  def stuff(K : Double)(F : Double) : Double
}
case object Call extends CallOrPut{
  override def toString = "Call"
  def toChar = 'C'
  def money(K : Double)(F : Double) = if (F > K) -K else 0.0
  def stuff(K : Double)(F : Double) = if (F > K) F else 0.0
}

case object Put extends CallOrPut{
  override def toString = "Put"
  def toChar = 'P'
  def money(K : Double)(F : Double) = if (F < K) K else 0.0
  def stuff(K : Double)(F : Double) = if (F < K) -F else 0.0
}

case object Straddle extends CallOrPut{
  override def toString = "Straddle"
  def toChar = 'S'
  def money(K : Double)(F : Double) = if (F > K) Call.money(K)(F) else Put.money(K)(F)
  def stuff(K : Double)(F : Double) = if (F > K) Call.stuff(K)(F) else Put.stuff(K)(F)
}

object CallOrPut {
  def unapply(s: String): Option[CallOrPut] = s.toLowerCase match {
      case "c" | "call" => Some(Call)
      case "p" | "put" => Some(Put)
      case other => None
  }
}