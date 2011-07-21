package starling.pivot

import java.io.Serializable

import starling.quantity.{Percentage => APercentage, UOM, Quantity => AQuantity}
import starling.utils.Pattern._


case class MarketValue(value: Either[AQuantity, APercentage]) extends Serializable {
  lazy val (quantity, percentage) = (value.left.toOption, value.right.toOption)
  override def toString = value.fold(_.toString, _.toString)
  def toQuantity = value.fold(identity, _.toQuantity)
  def pivotValue = value.fold(_.pq, identity)
}

object MarketValue {
  def quantity(value: AQuantity): MarketValue = new MarketValue(Left(value))
  def quantity(value: Double, uom: UOM): MarketValue = quantity(AQuantity(value, uom))
  def percentage(value: APercentage): MarketValue = new MarketValue(Right(value))
  def percentage(value: Double): MarketValue = percentage(APercentage(value))

  def fromString(value: String): MarketValue = value.trim match {
    case APercentage.Regex(value)    => percentage(value.toDouble / 100.0)
    case AQuantity.Regex(value, uom) => quantity(AQuantity.fromString(value.replace(",", ""), UOM.fromString(uom)))
    case trimmed                     => quantity(AQuantity.fromString(value.replace(",", ""), UOM.SCALAR))
  }

  val Quantity   = Extractor.from[MarketValue](_.quantity)
  val Percentage = Extractor.from[MarketValue](_.percentage)
}