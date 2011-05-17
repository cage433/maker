package starling.market

import starling.quantity.UOM
import starling.daterange.{DayAndTime, DateRange, TenorType}

/**
 * Represents something which has associated implied vols. For example forward markets and
 * FX rates.
 */
trait HasImpliedVol {
  self: Market =>

  val name: String
  def priceUOM : UOM
  def positionUOM : UOM
  def volatilityID: Option[Int] = None
}

/**
 * Represents an arbitrary FX market. Used for looking up vols. At present no relationship is imposed on
 * the vols of different exchange rates - even USD/GBP and GBP/USD are independent. This is of course wrong but should
 * work for now as the vols are stored with each FX option. this will need to be changed when we read vols from further upstream.
 */
case class FXMarket(numeratorCcy : UOM, denominatorCcy : UOM) extends Market with HasImpliedVol{
  val name = numeratorCcy + "/" + denominatorCcy
  val uomName = name
  def priceUOM = numeratorCcy / denominatorCcy
  def positionUOM = denominatorCcy
  def inverse = copy(denominatorCcy = numeratorCcy, numeratorCcy = denominatorCcy)
}
object FXMarket{
   lazy val allExchangeRates = for (numeratorCCY <- UOM.currencies;
                                    denominatorCCY <- UOM.currencies.filterNot(_ == numeratorCCY))
                                 yield FXMarket(numeratorCCY, denominatorCCY)

  /**
   * Parses text in the form e.g. EUR/USD
   */
  def fromString(text : String) = FXMarket(UOM.fromString(text.substring(0, 3)),  UOM.fromString(text.substring(4, 7)))

  def apply(rateUnit : UOM) : FXMarket = FXMarket(rateUnit.numeratorUOM, rateUnit.denominatorUOM)
}

