package starling.varcalculator


import starling.quantity.{UOM, Quantity}
import starling.market.{CommodityMarket, ForwardPriceRiskFactorType, VaRRiskFactorType, RiskFactorType}
import starling.quantity.utils.SummingMap

case class MarketPositions private (positions : SummingMap[VaRRiskFactorType]){
  positions.keySet.foreach(
    mkt => require(
      positions(mkt).uom == mkt.positionUOM,
      "Invalid market uom " + positions(mkt).uom + ", expected " + mkt.positionUOM
    )
  )
  def this() = this(SummingMap.empty[VaRRiskFactorType])
  def + (rhs : MarketPositions) = MarketPositions(positions ++ rhs.positions)

  def + (x : (VaRRiskFactorType, Quantity)) = {
    MarketPositions(positions + x)
  }
  def * (x : Double) = MarketPositions(positions * x)
  def size : Int = positions.size

  def netPosition = {
    val tonnes = positions.underlying.map {
      case (ForwardPriceRiskFactorType(m:CommodityMarket), v) => m.convert(v, UOM.MT) match {
        case Some(converted) => converted
        case None => Quantity.NULL // we can't sum things that can't be converted to tonnes
      }
      case (_, _) => Quantity.NULL // we can't sum into tonnes things that aren't prices
    }
    Quantity.sum(tonnes)
  }
}