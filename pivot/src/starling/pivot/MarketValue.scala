package starling.pivot


import java.io.Serializable
import starling.quantity.{Percentage, UOM, Quantity}


case class MarketValue(value: Either[Quantity, Percentage]) extends Serializable {
  override def toString = value.fold(_.toString, _.toString)
  def toQuantity = value.fold(identity, _.toQuantity)
  def pivotValue = value.fold(_.pq, identity)
}

object MarketValue {
  def quantity(value: Quantity): MarketValue = new MarketValue(Left(value))
  def quantity(value: Double, uom: UOM): MarketValue = quantity(Quantity(value, uom))
  def percentage(value: Percentage): MarketValue = new MarketValue(Right(value))
  def percentage(value: Double): MarketValue = percentage(Percentage(value))
}