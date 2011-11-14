package starling.curves
import starling.marketdata.ShanghaiVATData
import starling.marketdata.ShanghaiVATDataKey
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.daterange.DayAndTime

case class ShanghaiVATCurveObject(marketDayAndTime : DayAndTime, rate : Quantity) extends CurveObject{
  type CurveValuesType = Quantity
  def apply(point : AnyRef) = rate
}

case class ShanghaiVATCurveKey() extends NonHistoricalCurveKey[ShanghaiVATData]{
  def marketDataKey = ShanghaiVATDataKey()
  def buildFromMarketData (marketDayAndTime : DayAndTime, data : ShanghaiVATData) = {
    ShanghaiVATCurveObject(marketDayAndTime, data.rate)
  }
  def underlying = "Shanghai VAT"
}

case class ShanghaiVATRateKey()
  extends AtomicDatumKey(ShanghaiVATCurveKey(), "Dummy")
{
  def nullValue = Quantity(17, PERCENT)
    
  def forwardStateValue(original : AtomicEnvironment, forwardDayAndTime : DayAndTime) = original(this)

}
