package starling.instrument

import starling.daterange.{Month, DateRange}
import starling.market.{CommodityMarket, FuturesMarket}
import starling.quantity.{Percentage, UOM, Quantity}

case class FreightVolume(
        marketUOM: UOM,
        volume: Either[Quantity, Percentage],
        period: DateRange
        ) {
  def months = (Set.empty ++ period.map(_.containingMonth)).toList

  def amount(month: Month): Quantity = volume match {
    case Right(p) => Quantity(p.decimalValue * month.days.size, marketUOM)
    case Left(q) => q
  }

  def totalAmount = months.map(amount).reduceLeft(_+_)
}

object FreightVolume {
  def unit(uom: UOM) = new FreightVolume(null, null, null) {
    override def amount(month: Month) = Quantity(1, uom)

    override def totalAmount = throw new Exception("Doesn't really make sense")
  }
}