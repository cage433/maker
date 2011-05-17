package starling.instrument

import starling.daterange.{Month, DateRange}
import starling.quantity.{UOM, Quantity}
import starling.market.{CommodityMarket, FuturesMarket}

case class FreightVolume(
        marketUOM: UOM,
        volume: Quantity,
        period: DateRange
        ) {
  def months = (Set.empty ++ period.map(_.containingMonth)).toList

  def amount(month: Month): Quantity = volume.uom match {
    case UOM.PERCENT => Quantity(volume.value / 100.0 * month.days.size, marketUOM)
    case _ => volume
  }

  def totalAmount = months.map(amount).reduceLeft(_+_)
}

object FreightVolume {
  def unit(uom: UOM) = new FreightVolume(null, null, null) {
    override def amount(month: Month) = Quantity(1, uom)

    override def totalAmount = throw new Exception("Doesn't really make sense")
  }
}