package starling.curves.readers

import starling.quantity.Quantity
import starling.market.{Aluminium, Commodity}
import starling.marketdata._

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._


object NeptuneCommodity {
  val CommodityNameToCommodity = Commodity.CommodityNameToCommodity + ("Primary Aluminium" → Aluminium)
}

case class NeptuneCommodity(name: String) {
  import NeptuneCommodity._

  def countryBenchmarkKey: MarketDataKey = CountryBenchmarkMarketDataKey(commodity)
  def gradeAreaBenchmarkKey: MarketDataKey = GradeAreaBenchmarkMarketDataKey(commodity)
  def toQuantity(price: Double) = Quantity(price, commodity.representativeMarket.priceUOM)
  def commodity: Commodity = CommodityNameToCommodity.getOrElse(name, throw new Exception("Invalid " + this))
  def isValid = errorMessage.isEmpty
  override def toString = "Neptune Commodity: " + errorMessage.fold(error => name + " " + error, name)

  private lazy val errorMessage: Option[String] = (name, CommodityNameToCommodity.get(name)) partialMatch {
    case (null, _) => "(Null)"
    case (_, None) => "(Unknown)"
    case (_, Some(commodity)) if !commodity.hasRepresentativeMarket => "(No representative market)"
  }
}