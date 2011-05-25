package starling.varcalculator


import cern.colt.matrix.{DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import starling.curves._
import starling.maths.PriceShiftGenerator
import starling.quantity.UOM._
import starling.utils.conversions.RichColtMatrices._
import starling.quantity.Quantity
import starling.daterange.{DayAndTime, Day}
import starling.market.{VaRRiskFactorType, Market}

/**
 * Responsible for creating random environments using a set of RiskFactors - currently these are just prices
 * and spot fx rates.
 */
case class RiskFactorScenarioGenerator(
   originalEnv : Environment,
   riskFactors : List[VaRRiskFactor],
   prices : Vector,
   vols : Vector,
   rhoMatrix : Matrix,
   seed : Int,
   fallbackEnv: Option[Environment] = None // we can fall back to this environment for missing oil vols.
)
  extends ScenarioGenerator
{
  assert(riskFactors.forall(_.riskFactorType.isInstanceOf[VaRRiskFactorType]), "Not all risk factors are of type VaRRiskFactorType. " + riskFactors.map(_.riskFactorType.getClass).toSet)

  val dT = VarConstants.dT
  val psg = PriceShiftGenerator(prices, vols, rhoMatrix, dT, seed)
  def this(originalEnv : Environment, rfs : RiskFactorStatistics, seed : Int) =
    this(originalEnv, rfs.riskFactors, rfs.pricesVector, rfs.volsVector, rfs.rhoMatrix, seed)

  def this(originalEnv : Environment, fallbackEnv : Environment, rfs : RiskFactorStatistics, seed : Int) =
    this(originalEnv, rfs.riskFactors, rfs.pricesVector, rfs.volsVector, rfs.rhoMatrix, seed, Some(fallbackEnv))

  private val marketDayAndTime = originalEnv.marketDay

  private def shiftedPrices = prices + psg.nextShifts

  private def buildEnvironment(newPrices : Vector) : Environment = {

    var curveObjectMap = RiskFactorScenarioGenerator.buildCurveObjects(marketDayAndTime, riskFactors, newPrices)

    val origEnv = fallbackEnv match {
      case Some(fb) => FallbackCurveObjectEnvironment(originalEnv.atomicEnv, fb.atomicEnv, {
        case k:OilAtmVolAtomicDatumKey if k.market == Market.NYMEX_WTI => true
        case k:OilVolSkewAtomicDatumKey if k.market == Market.NYMEX_WTI => true
        case _ => false
      })
      case None => originalEnv.atomicEnv
    }

    Environment(CachingAtomicEnvironment(OverrideCurveObjectEnvironment(origEnv, curveObjectMap)))//.setProperty(AtomicEnvironment.PERMIT_IGNORE_SHIFTS)
  }
  def next : Environment = buildEnvironment(shiftedPrices)//.forwardState(marketDayAndTime.day.nextDay.startOfDay)
  lazy val originalBucketedEnv : Environment = buildEnvironment(prices.copy)//.forwardState(marketDayAndTime.day.endOfDay)
}

object RiskFactorScenarioGenerator {
  def buildCurveObjects(
          marketDayAndTime: DayAndTime, riskFactors: List[VaRRiskFactor], prices: Vector
          ): Map[CurveKey, CachingCurveObject] = {
    var map = Map.empty[CurveKey, CachingCurveObject]

    val riskFactorsByType: Map[VaRRiskFactorType, List[(VaRRiskFactor, Int)]] = {
      var map = Map.empty[VaRRiskFactorType, List[(VaRRiskFactor, Int)]]
      riskFactors.zipWithIndex.foreach {
        case (rf , index) =>
          val rfType = rf.riskFactorType.asInstanceOf[VaRRiskFactorType]
          val valueList = (rf, index) :: map.getOrElse(rfType, List[(VaRRiskFactor, Int)]())
          map += ((rfType, valueList)) // scala bug #1974
      }
      map
    }
    riskFactorsByType.keysIterator.foreach {
      rfType =>
        val riskFactorPrices: Map[VaRRiskFactor, Double] = Map.empty ++ riskFactorsByType(rfType).map {
          case (rf, iPrice) => rf -> prices(iPrice)
        }

        val curveObject = rfType.buildCurveObject(marketDayAndTime, riskFactorPrices)
        map += ((rfType.curveKey, CachingCurveObject(curveObject)))
    }
    map
  }

  def buildRandomScenarioGenerator(
    marketDay : Day,
    riskFactors : List[VaRRiskFactor],
    prices : Vector,
    vols : Vector,
    rhoMatrix : Matrix,
    seed : Int
  ) : RiskFactorScenarioGenerator =
  {
    val env = Environment(
      CachingAtomicEnvironment(
        OverrideCurveObjectEnvironment(
          MappingAtomicEnvironment(
            Map.empty[AtomicDatumKey, Quantity],
            marketDay.endOfDay
            ),
          buildCurveObjects (marketDay.endOfDay, riskFactors, prices)
          )
        )
      ).undiscounted
    new RiskFactorScenarioGenerator(env, riskFactors, prices, vols, rhoMatrix, seed)
  }
}


