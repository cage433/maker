package starling.market

import starling.daterange.{DayAndTime, Day}
import starling.pivot._
import starling.quantity.{Quantity, UOM}
import collection.SortedMap
import starling.curves._
import collection.immutable.TreeMap
import starling.daterange.TenorType
import starling.marketdata.{ReferenceDataLookup, MarketDataKey, MarketData, MarketDataType}

/**
 */

//just a marker to find this file
object SpecialSitsEquities

//A ric code to identify an equity (eg. AEM, PGM.TO, APC)
case class RIC(code:String) {
  override def toString = code
  //we need to know the currency to know what shift to apply for delta and VAR
  //I couldn't find a database of RICs but in a RIC the exchange follows the . and the currency for the exchange seems constant
  //so I'm hard coding the currencies for the exchange for now
  def currency = {
    val dot = code.indexOf(".")
    if (dot == -1) {
      code match {
        //case "african nickel" => UOM.CAD
        //case "mitra" => UOM.CAD
        case "Fisrt Coal" => UOM.CAD
        case "Sagex" => UOM.NOK
        case "SothC" => UOM.GBP
        case "Toro Gold" => UOM.GBP
        case "VDM" => UOM.GBP
        case _ => UOM.USD
      }
    } else {
      val exchange = code.substring(dot+1)
      exchange match {
        case "TO" => UOM.CAD
        case "O" => UOM.USD
        case "N" => UOM.USD
        case "L" => UOM.GBP
        case "V" => UOM.CAD
        case "AX" => UOM.AUD
        case "WA" => UOM.PLN
        case _ => UOM.USD
      }
    }
  }
  val priceUOM = currency / UOM.SHARE
}
object RIC {
  implicit object ordering extends Ordering[RIC]{
    def compare(lhs : RIC, rhs : RIC) : Int = lhs.code.compare(rhs.code)
  }
}

object EquityPricesDataType extends MarketDataType {
  val initialPivotState = PivotFieldsState(dataFields=List(Field("Price")), columnFields=List(Field("RIC")))
  val ricField = FieldDetails("RIC")
  val priceField = new AveragePivotQuantityFieldDetails("Price")
  val fields = List(ricField, priceField)
  val keys = List(EquityPricesMarketDataKey)
  def marketDataKeyFields = Set(ricField.field)
  def keyFields = Set(ricField.field)
  def valueFields = List(priceField.field)
  def createKey(row: Row) = EquityPricesMarketDataKey
  def createValue(rows: List[Row]) = EquityPrices(rows.map { row =>
    RIC(row.string(ricField)) → row.pivotQuantity(priceField).quantityValue.get
  } )
  type dataType = EquityPrices
}

object EquityPrices {
  def apply(traversable: Traversable[(RIC, Quantity)]): EquityPrices =
    EquityPrices(TreeMap.empty[RIC,Quantity](RIC.ordering) ++ traversable)
}

case class EquityPrices(prices:SortedMap[RIC,Quantity]) extends MarketData {

  def apply(ric:RIC) = {
    val price = prices.getOrElse(ric, throw new MissingMarketDataException("No price for " + ric + " Avaliable prices are: " + prices.keySet))
    assert(ric.priceUOM == price.uom, ric.priceUOM + " != " + price.uom + " for " + ric)
    price    
  }

  def size = prices.size
}

case object EquityPricesMarketDataKey extends MarketDataKey {
  type marketDataType = EquityPrices
  type marketDataDBType = EquityPrices
  def dataType = EquityPricesDataType
  def subTypeKey = ""
  override def rows(data : EquityPrices, referenceDataLookup: ReferenceDataLookup) = data.prices.map {
    case(ric,price) => Row(
      Field("RIC") -> ric.code,
      Field("Price") -> price.pq
    )
  }
  def fieldValues(referenceDataLookup: ReferenceDataLookup) = Map()
}

case class EquityPriceCurveObject(val marketDayAndTime:DayAndTime, ric:RIC, price:Quantity) extends CurveObject {
  assert(ric.priceUOM == price.uom, ric.priceUOM + " != " + price.uom + " for " + ric)
  def apply(point: AnyRef) = {
    price
  }

  type CurveValuesType = Quantity
}

case class EquityPriceCurveKey(ric:RIC) extends NonHistoricalCurveKey[EquityPrices] {
  def marketDataKey = EquityPricesMarketDataKey
  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: EquityPrices) = {
    new EquityPriceCurveObject(marketDayAndTime, ric, marketData(ric))
  }

  def underlying = ric.code
}

case class EquityPriceKey(ric:RIC) extends AtomicDatumKey(EquityPriceCurveKey(ric), None) with EnvironmentDifferentiable{
  type ValueType = Quantity
  def nullValue = Quantity(1, ric.priceUOM)
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv.quantity(this)
  }

  def periodKey = None

  def calc_dP(env : Environment) = Quantity(1.0, ric.priceUOM)

  def shiftedEnvs(env : Environment, dP : Quantity) : (Environment, Environment) = {
    val upEnv = env.shiftEquityPrice(ric, dP)
    val downEnv = env.shiftEquityPrice(ric, -dP)
    (downEnv, upEnv)
  }


  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = this
}
