package starling.market

import starling.quantity.UOM._
import starling.curves._
import starling.daterange._
import starling.quantity.{Quantity, Percentage}
import org.testng.annotations.AfterClass
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.utils.StarlingTest

trait JonTestEnv extends TestMarketSpec {
  def makeEnv(marketDay: DayAndTime, dVol: Double = 0.0, dPrice: Quantity = Quantity.NULL, dStdDev: Quantity = Quantity.NULL) = {
    import JonTestData._

    val wti = Market.NYMEX_WTI
    val rbob = Market.NYMEX_GASOLINE
    val index = Index.WTI10
    val brent = Index.BRT11
    val brentMarket = Index.BRT11.market
    val datedBrent = Index.DATED_BRENT
    val datedBrentMarket = datedBrent.market
    Environment(
      new UnitTestingAtomicEnvironment(marketDay, {
        case DiscountRateKey(`USD`, day, _) => math.exp(-interp(day) * day.daysSinceInYears(marketDay.day))
        case ForwardPriceKey(`wti`, forwardDate, _) => {
          Quantity(wtiForward(forwardDate.asInstanceOf[Month]), wti.priceUOM) + dPrice
        }
        case ForwardPriceKey(`rbob`, forwardDate, _) => {
          Quantity(rbobForward(forwardDate.asInstanceOf[Month]), rbob.priceUOM) + dPrice
        }
        case ForwardPriceKey(`brentMarket`, forwardDate, _) => {
          val month = forwardDate.asInstanceOf[Month]
          val carry = month.lastDay.daysSinceInYears(Day(2011, 2, 1))
          Quantity(100 + carry, brent.priceUOM) + dPrice
        }
        case ForwardPriceKey(`datedBrentMarket`, forwardDate, _) => {
          val day = forwardDate.asInstanceOf[Day]
          val carry = day.daysSinceInYears(Day(2011, 2, 1))
          Quantity(99.1965 + carry, datedBrent.priceUOM) + dPrice
        }
        case FixingKey(`index`, d) => {
          Quantity(wtiForward(d.containingMonth), wti.priceUOM)
        }
        case FixingKey(`brent`, d) => {
          Quantity(wtiForward(d.containingMonth), brent.priceUOM)
        }
        case OilAtmVolAtomicDatumKey(`wti`, _, period, _) => {
          val wtiATMVols1 = wtiATMVols(period.asInstanceOf[Month])
          Percentage(wtiATMVols1) + Percentage(dVol)
        }
        case OilVolSkewAtomicDatumKey(`wti`, period) => {
          val ovs = wtiSkewDeltas.zip(wtiSkewValues(period.asInstanceOf[Month]).map(Percentage(_))).toMap
          ovs
        }
        case SpreadAtmStdDevAtomicDatumKey(_, _, _) => {
          Quantity(1.5, wti.priceUOM) + dStdDev
        }
        case SpreadSkewStdDevAtomicDatumKey(market, period) => {
          val matrix = new DenseDoubleMatrix2D(1, 2)
          matrix.set(0, 0, 0.15)
          matrix.set(0, 1, 0.8)
          matrix
        }
      })
      )
  }

  def makeEnvShift(marketDay: DayAndTime, dVol: Map[DateRange, Double] = Map.empty, dPrice: Map[DateRange, Quantity] = Map.empty,
                   dStdDev: Map[Spread[_ <: DateRange], Quantity] = Map.empty, dStdDevSkew: Map[Spread[_ <: DateRange], Double] = Map.empty) = {
    import JonTestData._

    val market = Market.NYMEX_WTI
    val index = Index.WTI10
    Environment(
      new UnitTestingAtomicEnvironment(marketDay, {
        case DiscountRateKey(`USD`, day, _) => math.exp(-interp(day) * day.daysSinceInYears(marketDay.day))
        case ForwardPriceKey(`market`, forwardDate, _) => {
          Quantity(wtiForward(forwardDate.asInstanceOf[Month]), market.priceUOM) + dPrice.getOrElse(forwardDate, Quantity.NULL)
        }
        case OilAtmVolAtomicDatumKey(`market`, _, period, _) => {
          val wtiATMVols1 = wtiATMVols(period.asInstanceOf[Month])
          Percentage(wtiATMVols1) + Percentage(dVol.getOrElse(period, 0.0))
        }
        case OilVolSkewAtomicDatumKey(`market`, period) => {
          val ovs = wtiSkewDeltas.zip(wtiSkewValues(period.asInstanceOf[Month]).map(Percentage(_))).toMap
          ovs
        }
        case SpreadAtmStdDevAtomicDatumKey(_, period, _) => {
          Quantity(1.5, market.priceUOM) + dStdDev.getOrElse(period, Quantity.NULL)
        }
        case SpreadSkewStdDevAtomicDatumKey(market, period) => {
          val matrix = new DenseDoubleMatrix2D(1, 2)
          matrix.set(0, 0, 0.15 + dStdDevSkew.getOrElse(period, 0.0))
          matrix.set(0, 1, 0.8 + dStdDevSkew.getOrElse(period, 0.0))
          matrix
        }
      })
      )
  }
}
