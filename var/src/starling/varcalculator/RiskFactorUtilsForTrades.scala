package starling.varcalculator

import starling.market._
import starling.instrument.Instrument
import starling.quantity.UOM
import starling.daterange._
import starling.curves.{MissingMarketDataException, Environment}
import collection.immutable.TreeSet

object RiskFactorUtilsForTrades {
  import RiskFactorUtils._

  def bucketedVaRTradeRiskFactors(trades: Seq[Instrument], env: Environment, valuationCCY: UOM): Set[VaRRiskFactor] = {
    val riskFactorSet = varRiskFactorsForTrades(trades, env, valuationCCY)
    bucketVaRRiskFactors(env.marketDay, riskFactorSet)
  }

  def varRiskFactorsForTrades(trades: Seq[Instrument], env: Environment, valuationCCY: UOM): Set[VaRRiskFactor] = {
    (Set[VaRRiskFactor]() /: trades)({
      (riskFactors, trade) => {
        try {
          riskFactors ++ trade.varRiskFactors(env, valuationCCY)
        } catch {
          //This catch all is dangerous but without it the reference var fails completely
          case e: Exception => riskFactors
        }

      }
    })
  }

  def bucketedTradeRiskFactors(trades: Seq[Instrument], env: Environment, valuationCCY: UOM): Set[RiskFactor] = {
    val riskFactorSet = riskFactorsForTrades(trades, env, valuationCCY)
    bucketRiskFactors(env.marketDay, riskFactorSet)
  }

  def riskFactorsForTrades(trades: Seq[Instrument], env: Environment, valuationCCY: UOM): Set[RiskFactor] = {
    (Set[RiskFactor]() /: trades)({
      (riskFactors, trade) => {
        try {
          riskFactors ++ trade.riskFactors(env, valuationCCY)
        } catch {
          //This catch all is dangerous but without it the reference var fails completely
          case e: Exception => riskFactors
        }

      }
    })
  }
}

