package starling.instrument

import starling.curves.{Environment}
import starling.quantity.{UOM, Quantity}
import starling.daterange._
import starling.curves.EnvironmentDifferentiable
import starling.curves.PriceKey
import starling.curves.VolKey
import starling.instrument.utils.AtomicDatumKeyUtils._
import starling.utils.ImplicitConversions._
import starling.utils.cache.CacheFactory


trait UTP extends Instrument {
  def instrumentType : InstrumentType[_]

  def detailsForUTPNOTUSED : Map[String, Any]

  def fields:List[String] = {
    val tradeableFields = detailsForUTPNOTUSED.keySet.map(_.removeWhiteSpace.toLowerCase).toList
    val allConvertedFields = InstrumentType.fields.map(_.removeWhiteSpace.toLowerCase)
    val matchingFields = allConvertedFields.intersect(tradeableFields)
    matchingFields.map(field => InstrumentType.fields(allConvertedFields.indexOf(field)))
  }

  /**
   * For the provided environment what will this be on the provided future date.
   * For example, for an forward option if it is in the money after the exercise day it will be a forward.
   */
  def forwardState(env:Environment, dayAndTime:DayAndTime): UTP = this

  override def theta(env: Environment, thetaDayAndTime:DayAndTime, ccy : UOM, changeOnlyTimeAndDiscounts : Boolean = false): Quantity = {
    val (envToday, forwardEnv) = Greeks.envFor(env, thetaDayAndTime, changeOnlyTimeAndDiscounts = changeOnlyTimeAndDiscounts)
    val mtmNow = cachedMtm(envToday, UOM.USD)
    val forwardInstrument = this.forwardState(envToday, thetaDayAndTime)
    val forwardMtm = forwardInstrument.cachedMtm(forwardEnv, UOM.USD)

    forwardMtm - mtmNow
  }

  /**
   * Needed for the delta report - works for all current instruments but if there is
   * no meaningful answer for a nw instrument then this report will have to be rethought
   */
  def volume : Quantity

  /**
   * Days with which to split up positions/gammas etc
   */
  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day]

  def * (scale : Double) : UTP

  /**
   * The EnvironmentDifferentiables used in greeks, p&l pivot reports
   * Some UTPs will override this, e.g. swaps and APOs will return the SwapPrice, rather than the underlying futures prices
   */
  def priceAndVolKeys(marketDay : DayAndTime) : (Set[EnvironmentDifferentiable with PriceKey], Set[EnvironmentDifferentiable with VolKey]) = {
    UTP.priceAndVolKeys(this, marketDay)
  }

  /**
   * Used (I believe) to give a crude idea of risk period in Greeks Pivot Report. For most instruments this will be overriden
   * by their price/vol sensitivity periods. Not convinced this is needed.
   */
  def periodKey : Option[Period]

  // This string is added to risk market when the 'Collapse Option' pivot toggle is off
  def riskMarketExtra : String  = ""

  def asUnitUTP : (UTP, Double) = {
    if (volume.isZero)
      (this, 1.0)
    else
      (this * (1.0 / volume.value), volume.value)
  }

  // Determining atomic keys is done a lot and cached - to avoid unnecessary recalculation of similar futures options
  // we can do the same calculation on an option with a specific strike and cache that
  def atomicKeyCachingUTP : UTP = this

  // Used in the GreeksPivotReport to display an associated price
  def price(env : Environment) : Quantity
}

object UTP{
  def extraPivotDetails =  Map[String, Any]()
  
  private var priceAndVolKeysCache = CacheFactory.getCache("UTP.priceAndVolKeysCache", unique = true)

  def priceAndVolKeys(
    utp : UTP, 
    marketDay : DayAndTime, 
    showEquivalentFutures : Boolean = true, 
    tenor : TenorType = Day
  )
    : (Set[EnvironmentDifferentiable with PriceKey], Set[EnvironmentDifferentiable with VolKey]) = {
    val cachingUTP = utp.atomicKeyCachingUTP
    priceAndVolKeysCache.memoize(
      (
        // for options the strike price is immaterial - to save unnecessary recaclulation we set the strike to 1
        cachingUTP,
        marketDay, 
        showEquivalentFutures, 
        tenor
      ),
    (_ : (UTP, DayAndTime, Boolean, TenorType)) => {
      var (pKeys, vKeys) = (utp, showEquivalentFutures) match {
        case (_, true) => (priceKeys(utp, marketDay, UOM.USD), volKeys(utp, marketDay, UOM.USD))
        case (_, false) => cachingUTP.priceAndVolKeys(marketDay)
      }

      pKeys = pKeys.map(_.containingDifferentiable(marketDay, tenor).asInstanceOf[EnvironmentDifferentiable with PriceKey])
      vKeys = vKeys.map(_.containingDifferentiable(marketDay, tenor).asInstanceOf[EnvironmentDifferentiable with VolKey])
      (pKeys, vKeys)
    })
  }

}
