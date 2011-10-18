package starling.instrument

import starling.daterange.Period
import starling.marketdata.MarketData
import cern.colt.matrix.DoubleMatrix2D
import starling.instrument.utils.AtomicDatumKeyUtils
import starling.quantity.{Percentage, UOM, Quantity}
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.curves._
import starling.utils.CollectionUtils
import starling.pivot.PivotQuantity

case class CurveKeyExplanation(curveKeys: List[CurveKey], riskMarket : String, period: Option[Period], order: Option[Int], value: PivotQuantity, priceChange : Option[PivotQuantity], d1Price : Option[PivotQuantity], volChange : Option[PivotQuantity]){
  def riskCommodity = Some(curveKeys.head.higherUnderlying)
  def riskType = Some(curveKeys.head.typeName.name)
}

trait PnlExplanation {
  self: Instrument with Greeks =>

  /**
   * Explains PnL change for this instrument from d1 to d2
   *
   * @return A list of CurveKeyExplanations, each one explains a curve key for an option period.
   */
  def explain(d1EnvFwd: Environment, d2Env: Environment,
              environmentFor: (Set[CurveKey]) => Environment,
              ccy: UOM): List[CurveKeyExplanation] = {
    val envDiffs = AtomicDatumKeyUtils.environmentDifferentiables(this, d1EnvFwd.marketDay, ccy)
    val curveKeys = AtomicDatumKeyUtils.curveKeys(this, d1EnvFwd.marketDay, ccy)
    explain(d1EnvFwd, d2Env, environmentFor, ccy, envDiffs, curveKeys)
  }

 /**
   * Explains PnL change for this instrument from d1 to d2
   *
   * @return A list of CurveKeyExplanations, each one explains a curve key for an option period.
   */
  def explain(
    d1EnvFwd: Environment, d2Env: Environment,
    environmentFor: (Set[CurveKey]) => Environment,
    ccy: UOM,
    diffs : Set[EnvironmentDifferentiable],
    curveKeys : Set[CurveKey],
    atmVega : Boolean = true
  ): List[CurveKeyExplanation] = {
    assert(d1EnvFwd.marketDay == d2Env.marketDay, "The market days should be the same: " + (d1EnvFwd.marketDay, d2Env.marketDay))
    assert(diffs.forall{d => curveKeys.contains(d.curveKey)}, "Missing a curve key for some environment differentiabls")
    val d1Mtm = PivotQuantity.calcOrCatch(cachedMtm(d1EnvFwd, ccy))

    val relatedCurveKeys : List[List[CurveKey]]= curveKeys.filter(_ match {
      case _: OilVolSkewCurveKey => false
      case _: SpreadSkewStdDevCurveKey => false
      case _ => true
    }).map(_ match {
      case k@OilAtmVolCurveKey(market) => List(k, OilVolSkewCurveKey(market))
      case k@SpreadAtmStdDevCurveKey(market) => List(k, SpreadSkewStdDevCurveKey(market))
      case k => List(k)
    }).toList

    val diffsByCurveKey = relatedCurveKeys.map{
      case curveKeys => (curveKeys -> diffs.filter(_.curveKey == curveKeys.head))
    }.toMap
    assert(diffs == (Set[EnvironmentDifferentiable]() /: diffsByCurveKey.valuesIterator)(_++_), "Missing a curve key for some differentiable")


    // try to explain all the curve keys and the atomic datum keys for each curve key
    diffsByCurveKey.flatMap(x => (x : @unchecked) match {
      case (primaryCurveKey::otherCurveKeys, diffsForThisCurveKey) =>
        val curveKeys = (primaryCurveKey::otherCurveKeys)
        // the curve key pnl explains pnl change at a high level. For example the change due to the
        // whole price curve moving for a particular market
        val curveKeyPnl = {
          val curveKeyEnv = environmentFor(curveKeys.toSet)
          val mtmWithSubstitution = PivotQuantity.calcOrCatch(cachedMtm(curveKeyEnv, ccy))
          mtmWithSubstitution - d1Mtm
        }
        // the atomic datum key pnl tries to explain at a finer grain what the changes are. For example
        // the change due to Nymex WTI March '11 future prices moving.
        // Some atomic datum keys we won't be able to explain (for example skew maps) and they will throw
        // an exception, which we'll catch and then we'll add them to the unexplained list.
        val envDiffsPnl =
          diffsForThisCurveKey.toList.flatMap {
            envDiff => {
              val greeksPnl = {
                val (foChange, soChange, valueChange, d1PriceX) = try {
                  val (valueChange, d1Price, zeroVols) = (atmVega, envDiff) match {
                    case (false, volKey: EnvironmentDifferentiable with VolKey) => {
                      val d2Vol = interpolatedVol(d2Env, volKey)
                      val d1FwdVol = interpolatedVol(d1EnvFwd, volKey)
                      val change = d2Vol - d1FwdVol
                      (change, None, d2Vol.isZero && d1FwdVol.isZero)
                    }
                    case _ => {
                      val d1 = envDiff.quantityValue(d1EnvFwd)
                      val change = envDiff.quantityValue(d2Env) - d1
                      (change, Some(d1), false)
                    }
                  }
                  if(zeroVols) {
                    // bit of a hack but we have no way to access if 'ZeroVols' was checked in the GUI. If it has been checked
                    // we can't do numeric diff and we report no sensitivity to vol.
                    (PivotQuantity.NULL, PivotQuantity.NULL, PivotQuantity(valueChange), None)
                  } else {
                    val foDeriv = PivotQuantity.calcOrCatch(firstOrderDerivative(d1EnvFwd, envDiff, ccy, shiftInterpolatedVols = !atmVega))
                    val soDeriv = PivotQuantity.calcOrCatch(secondOrderDerivativeWithCrossTerm(d1EnvFwd, envDiff, ccy, diffsForThisCurveKey.toList, shiftInterpolatedVols = !atmVega))
                    val foExplained = (foDeriv * valueChange) inUOM ccy
                    val soExplained = (soDeriv * (valueChange * valueChange) / 2) inUOM ccy
                    (foExplained, soExplained, PivotQuantity(valueChange), d1Price.map(p => PivotQuantity(p)))
                  }
                } catch {
                  case e => {
                    (new PivotQuantity(e), new PivotQuantity(e), new PivotQuantity(e), None)
                  }
                }
                val (priceChange, d1Price, volChange) = envDiff match {
                  case _ : PriceKey => (Some(valueChange), d1PriceX, None)
                  case _ : VolKey => (None, None, Some(valueChange))
                  case _ => (None, None, None)
                }

                List(CurveKeyExplanation(curveKeys, envDiff.riskMarket, envDiff.periodKey, Some(1), foChange, priceChange, d1Price, volChange),
                  CurveKeyExplanation(curveKeys, envDiff.riskMarket, envDiff.periodKey, Some(2), soChange, priceChange, d1Price, volChange))

              }
              greeksPnl
            }
        }
        val highLevelPnl =
          CurveKeyExplanation(curveKeys, primaryCurveKey.underlying, None: Option[Period], None, curveKeyPnl - envDiffsPnl.map(_.value).sum, None, None, None)
        // Ignoring the highLevelPnl for now if we have envDiffsPnl, as JF doesn't want it on its own row. This will now go into
        // cross terms

        //highLevelPnl :: envDiffsPnl
        if (envDiffsPnl.isEmpty)
          List(highLevelPnl)
        else
          envDiffsPnl
      }).toList
  }

  /**
   * Breaks the explanation out into
   * - crossTerms - cross and higher order terms
   * - rounding - difference in pnl explanation due to rounding happening in valuation but not in the explanation
   * - unexplained
   */
  def components(
                  d1EnvFwd: Environment, d2Env: Environment,
                  environmentFor: (Set[CurveKey]) => Environment,
                  ccy: UOM,
                  explainedTotal: PivotQuantity,
                  curveKeys: Set[CurveKey]) = {
    val d1Mtm = PivotQuantity.calcOrCatch(cachedMtm(d1EnvFwd, ccy))
    val plainPnl = PivotQuantity.calcOrCatch(cachedMtm(d2Env, ccy)) - d1Mtm
    val allCurveKeysMtm = PivotQuantity.calcOrCatch({
      cachedMtm(environmentFor(curveKeys), ccy)
    })
    val allCurvesPnl = allCurveKeysMtm - d1Mtm
    val (rounding, crossTerms) = {
      val term = allCurvesPnl - explainedTotal
      priceRounding match {
        case Some(_) => {
          assert(isLinear(d1EnvFwd.marketDay), "This wasn't designed for explaining options that have rounding. Needs to be re-visited.")
          // we're assuming the cross term is due to rounding which is true for linear instruments with rounding.
          (term, PivotQuantity(0 (USD)))
        }
        case _ => (PivotQuantity(0 (USD)), term)
      }
    }
    val unexplained = plainPnl - allCurvesPnl

    val pnlExplanationTotal = explainedTotal + crossTerms + rounding + unexplained

    if(!plainPnl.hasWarningOrErrors && !pnlExplanationTotal.hasWarningOrErrors && !(plainPnl - pnlExplanationTotal).isAlmostZero){
      println("?????????/**/")
      println("?????????/**/" + (plainPnl + " != " + pnlExplanationTotal))
      println("?????????/**/")
    }
    // if we have no warnings or errors this is just a sanity check to make sure we have explained the actual pnl.
    if(!plainPnl.hasWarningOrErrors && !pnlExplanationTotal.hasWarningOrErrors)
      assume((plainPnl - pnlExplanationTotal).isAlmostZero, plainPnl + " != " + pnlExplanationTotal)

    (crossTerms, rounding, unexplained)
  }

}
