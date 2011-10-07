package starling.reports.impl.pivot.greeks

import starling.daterange._
import starling.instrument._
import starling.curves._
import starling.gui.api.UTPIdentifier
import starling.pivot.{PivotQuantity => PQ}
import starling.reports.pivot._
import greeks.{RiskPrices, VolatilityMeasure, RiskVols}
import starling.reports.impl.pivot.RiskPivotReportRow
import starling.market.KnownConversions

object GreekValues {
  def fields = List("Market Price", "Current Price", "Volatility", PositionText, "Gamma", "Vega", "Vomma", "DeltaBleed", "GammaBleed")
  def fromUTP(utpIdentifier : UTPIdentifier, utp : UTP) : GreekValues = {
    GreekValues(utpID = utpIdentifier, utp = utp, diff = None, period = utp.periodKey)
  }
  val PositionText = "Position"
}

case class GreekValues(
  utpID : UTPIdentifier,
  utp : UTP,
  diff: Option[EnvironmentDifferentiable],
  period : Option[Period] = None,
  price : PQ = PQ.NULL,
  instrumentPrice : PQ = PQ.NULL,
  volatility : PQ = PQ.NULL,
  position : PQ = PQ.NULL,
  gamma: PQ = PQ.NULL,
  vega: PQ = PQ.NULL,
  vomma: PQ = PQ.NULL,
  deltaBleed: PQ = PQ.NULL,
  gammaBleed: PQ = PQ.NULL,
  marketName : String = "",
  collapseOptions : Boolean = true,
  scale : Double = 1.0
)
  extends RiskPivotReportRow[GreekValues]
{
  import GreekValues._
  def get(field: String) = field match {
    case "Market Price" => new RiskPrices(marketName, period, price)
    case "Current Price" => instrumentPrice
    case "Volatility" => {
      volatility.quantityValue.map { v => {
        new RiskVols(marketName, period, VolatilityMeasure(v))
      }}.getOrElse(RiskVols.Null)
    }
    case PositionText => position * scale
    case "Gamma" => gamma * scale
    case "Vega" => vega * scale
    case "Vomma" => vomma * scale
    case "DeltaBleed" => deltaBleed * scale
    case "GammaBleed" => gammaBleed * scale
  }

  override def toString = {

    "utpID -> " + utpID + "\n" +
    "utp -> " + utp + "\n" +
    "diff -> " + diff+"\n" +
    "period -> " + period + "\n" +
    "position -> " + position + "\n" +
    "gamma -> " + gamma+"\n" +
    "vega -> " + vega+"\n" +
    "vomma -> " + vomma+"\n" +
    "deltaBleed -> " + deltaBleed+"\n" +
    "gammaBleed -> " + gammaBleed+"\n" +
    "scale -> " + scale
  }

  def conversion: Option[KnownConversions] = utp.knownConversion
  def *(volume: Double) = copy(scale = scale * volume)
  def riskType = diff.map(_.riskType)
  def riskCommodity = diff.map(_.riskCommodity)
  def scaledPosition = position * scale
  def setPeriod(period : Option[Period]) = copy(period = period)
}
