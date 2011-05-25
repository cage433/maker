package starling.varcalculator

import starling.market._
import starling.quantity.UOM
import starling.daterange._
import starling.curves.Environment
import javax.sound.midi.Instrument

object RiskFactorUtils {

  private def breakPriceRiskFactorIntoMonths(marketDayAndTime : DayAndTime, rf : ForwardPriceRiskFactor) : List[ForwardPriceRiskFactor] = {
    val frontDay = rf.market.nthPeriod(marketDayAndTime, 0).asInstanceOf[Day]
    var month : DateRange = rf.period(frontDay.endOfDay).firstMonth
    if (month.firstDay < frontDay)
      month = SimpleDateRange(frontDay, month.lastDay)
    
    val lastMonth = rf.period(marketDayAndTime).lastMonth
    var months = List[DateRange]()
    while (month.lastDay <= lastMonth.lastDay){
      months = month::months
      month = month.lastDay.nextDay.containingMonth
    }
    months.reverse.map{ m => ForwardPriceRiskFactor(rf.market, m.firstDay - frontDay, m.lastDay - frontDay)}
  }

  private def breakVolRiskFactorIntoMonths(marketDayAndTime : DayAndTime, rf : VolatilityRiskFactor, earliestVolPeriod: Day) : List[VolatilityRiskFactor] = {
    var month : DateRange = rf.period.firstMonth
    if (month.firstDay < earliestVolPeriod)
      month = SimpleDateRange(earliestVolPeriod, month.lastDay)

    val lastMonth = rf.period.lastMonth
    var months = List[DateRange]()
    while (month.lastDay <= lastMonth.lastDay){
      months ::= month
      month = month.lastDay.nextDay.containingMonth
    }
    months.reverse.map {m => VolatilityRiskFactor(rf.market, m)}
  }

  def bucketVaRRiskFactors(marketDayAndTime: DayAndTime, riskFactors: Set[VaRRiskFactor]): Set[VaRRiskFactor] = {
    // Sets are invariant so lots of casting fun
    bucketRiskFactors(marketDayAndTime, riskFactors.asInstanceOf[Set[RiskFactor]]).asInstanceOf[Set[VaRRiskFactor]]
  }

  /**
   * Unnecessary to have a risk factor for each price point, as within month prices tend to be very close,
   * if not identical, and are very highly correlated. Instead split into monthly buckets,
   * using for each bucket the longest dated original risk factor contained within.
   */
  def bucketRiskFactors(marketDayAndTime: DayAndTime, riskFactors: Set[RiskFactor]): Set[RiskFactor] = {
    val marketDay = marketDayAndTime.day
    var lastInterestRateDays = Map.empty[InterestRateMarket, Day]

    val vrfs = volRiskFactors(riskFactors)
    val dailyVols = vrfs.forall(_.period.isInstanceOf[Day])
//    val earliestVolPeriod = vrfs.map(_.period).sorted.head

    var bucketedRFS = Set[RiskFactor]()
    for (rf <- riskFactors) {
      rf match {
        case rf: ForwardPriceRiskFactor =>
          rf.riskFactorType match {
            case ForwardPriceRiskFactorType(m) => m.tenor match {
              case Month => bucketedRFS += rf // no need to bucket monthly markets 
              case Day => bucketedRFS ++= breakPriceRiskFactorIntoMonths(marketDayAndTime, rf)
            }
            case _ => throw new Exception("Unknown type " + rf)
          }
        case ForwardRateRiskFactor(market, nDaysToStart, nDaysToEnd) =>{
          val lastDay = marketDay + nDaysToEnd

          if (!lastInterestRateDays.contains(market) || lastInterestRateDays(market) < lastDay) {
            bucketedRFS = bucketedRFS.filter(_.riskFactorType != market) + ForwardRateRiskFactor(market, 0, nDaysToEnd)
            lastInterestRateDays = lastInterestRateDays.updated(market, lastDay)
          }
      }
//        case vrf: VolatilityRiskFactor if dailyVols => {
//          val earliestVolPeriod = vrfs.map(_.period).sorted.head
//          bucketedRFS ++= breakVolRiskFactorIntoMonths(marketDayAndTime, vrf, earliestVolPeriod.asInstanceOf[Day])
//        }
        case _ => bucketedRFS += rf
      }
    }
    bucketedRFS
  }

  /**
   * gets the appropriate vol risk factor for the given price risk factor
   */
  def volRiskFactor(pricePeriod: DateRange, riskFactors: Set[RiskFactor]): VolatilityRiskFactor = {
    val vrfs = volRiskFactors(riskFactors)

    // we can only handle 2 vol risk factors at the moment. if that becomes a problem this method
    // should be easy enough to rewrite, it was just much easier to write this way when there are currently
    // only ever 2 vol risk factors.
    assert(vrfs.size <= 2, "Too many vol risk factors")

    vrfs match {
      case (v@VolatilityRiskFactor(_, `pricePeriod`)) :: Nil => {
        // vol for the same period
        v
      }
      case (v@VolatilityRiskFactor(_, `pricePeriod`)) :: other => {
        // vol for same period with multiple vols
        v
      }
      case other :: (v@VolatilityRiskFactor(_, `pricePeriod`)) :: Nil => {
        // vol for same period with multiple vols
        v
      }
      case (v@VolatilityRiskFactor(_, _)) :: Nil => {
        //options can expire before futures we can have, for example, an asian
        // option that is sensitive to prices in May and Jun but only sensitive to
        // vol for Jun (as May vol is no longer available).
        // if this is the only vol risk factor then it is the right one
        v
      }
      case _ => throw new Exception("Shouldn't be here")
    }
  }

  def priceRiskFactors(riskFactors: Set[RiskFactor]): List[ForwardPriceRiskFactor] = riskFactors.filter {
    case _: ForwardPriceRiskFactor => true
    case _ => false
  }.toList.asInstanceOf[List[ForwardPriceRiskFactor]]

  def volRiskFactors(riskFactors: Set[RiskFactor]): List[VolatilityRiskFactor] = riskFactors.filter {
    case _: VolatilityRiskFactor => true
    case _ => false
  }.toList.asInstanceOf[List[VolatilityRiskFactor]]

  def hasVolSensitivity(riskFactors: Set[RiskFactor]) = !volRiskFactors(riskFactors).isEmpty
}
