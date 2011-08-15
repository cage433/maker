package starling.curves

import starling.db.MarketDataReader
import starling.reports.pivot.IndexRuleEvaluation
import starling.quantity.UOM._
import starling.pivot._
import starling.market.rules.{SwapPricingRule, NonCommonPricingRule, Precision}
import starling.utils.{AppendingMap, Log, Stopwatch}
import starling.daterange.{Day, DateRange, Month}
import starling.utils.cache.CacheFactory
import starling.market.formula.FormulaIndex
import starling.market._
import starling.maths.BrentSolver
import starling.models.BlackScholes
import starling.models.Call
import starling.quantity.{UOM, Percentage, Quantity}

/**
 * Curve viewer for Implied Vols
 */
object ImpliedVolCurveTypeFactory extends CurveType {
  def create(marketDataReader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, marketDataReader)

    new ImpliedVolPivotDataSource(context)
  }
}

class ImpliedVolPivotDataSource(context: EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {
  val period = FieldDetails("Period")
  val marketField = FieldDetails("Market")
  val delta: FieldDetails = new FieldDetails("Delta") {
    override def comparator = new Ordering[Any]() {
      def compare(x: Any, y: Any) = y.asInstanceOf[String].compare(x.asInstanceOf[String])
    }
  }
  val price = FieldDetails("Price")
  val volatility = FieldDetails("Volatility")
  val diff = FieldDetails("Difference")

  val env = context.environment.undiscounted
  val marketDay = context.environment.marketDay
  val markets = context.marketVols.flatMap(_.market match {
    case k: CommodityMarket with KnownExpiry => Some(k)
    case _ => None
  })
  val marketsData = context.marketVols.map(m => m.market -> m).toMap

  val fieldDetailsGroups = {
    FieldDetailsGroup("Fields", period, marketField, price, volatility, delta, diff) :: Nil
  }

  val marketSelection = marketsData.keys.headOption match {
    case Some(m) => SomeSelection(Set(m.name))
    case _ => AllSelection
  }

  override def initialState = {
    new PivotFieldsState(
      columns = ColumnTrees(
        List(ColumnTree(delta.field, false, List(price, volatility).map(f => ColumnTree(f.field, true)) : _*))),
      rowFields = List(period).map(_.field),
      filters = List((marketField.field, marketSelection))
    )
  }

  def unfilteredData(pfs: PivotFieldsState) = {
    markets.flatMap {
      market => {
        val marketData = marketsData(market)
        val data = marketData.oilVolSurfaceData
        data.periods.zipWithIndex.flatMap {
          case (dr, index) => {
            try {
              val atmVol = env.atmImpliedVol(market, dr)
              val expiry = market.optionExpiry(dr)
              val time = expiry.endOfDay.timeSince(marketDay)
              val F = env.forwardPrice(market, dr)
              val disc = env.discount(market.currency, expiry).checkedValue(UOM.SCALAR)

              fields(marketField -> market.name, period -> dr, delta -> "ATM", volatility -> (atmVol), price -> F) ::
                data.skewDeltas.toList.zipWithIndex.map {
                  case (skewDelta, skewIndex) => {
                    val undiscountedSkewDelta = skewDelta / disc
                    val K = new BlackScholes(F.value, 0, Call, time, atmVol.value).strikeFromDelta(undiscountedSkewDelta)
                    val strike = Quantity(K, market.priceUOM)
                    val vol = env.impliedVol(market, dr, expiry, strike)

                    val inputVol = atmVol.value + (data.skews(skewIndex)(index))
                    val volDiff = Percentage(vol.value - inputVol)

                    fields(marketField -> market.name, period -> dr,
                      delta -> skewDelta.toString, volatility -> vol,
                      price -> strike, diff -> volDiff)
                  }
                }
            } catch {
              case _ => None
            }
          }
        }
      }
    }.toList
  }
}