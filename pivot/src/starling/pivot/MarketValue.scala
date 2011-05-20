package starling.pivot

import java.io.Serializable

import starling.quantity.{Percentage, UOM, Quantity}


case class MarketValue(value: Either[Quantity, Percentage]) extends Serializable {
  lazy val (quantity, percentage) = (value.left.toOption, value.right.toOption)
  override def toString = value.fold(_.toString, _.toString)
  def toQuantity = value.fold(identity, _.toQuantity)
  def pivotValue = value.fold(_.pq, identity)
}

object MarketValue {
  def quantity(value: Quantity): MarketValue = new MarketValue(Left(value))
  def quantity(value: Double, uom: UOM): MarketValue = quantity(starling.quantity.Quantity(value, uom))
  def percentage(value: Percentage): MarketValue = new MarketValue(Right(value))
  def percentage(value: Double): MarketValue = percentage(starling.quantity.Percentage(value))
  object Quantity   { def unapply(value: MarketValue) = value.quantity   }
  object Percentage { def unapply(value: MarketValue) = value.percentage }
}