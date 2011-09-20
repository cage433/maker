package starling.curves

import starling.db.MarketDataReader
import starling.quantity.UOM._
import starling.pivot._
import starling.market.rules.{SwapPricingRule, NonCommonPricingRule, Precision}
import starling.utils.{AppendingMap, Log, Stopwatch}
import starling.daterange.{Day, DateRange, Month}
import starling.utils.cache.CacheFactory
import starling.market.formula.FormulaIndex
import starling.market._
import starling.quantity.{Percentage, Quantity}

object SwapVolScheduleFactory extends CurveType {
  def create(marketDataReader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, marketDataReader)

    new SwapVolSchedulePivotDataSource(context)
  }
}

class SwapVolSchedulePivotDataSource(context: EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {
  private val indexField = FieldDetails("Index")
  private val period = FieldDetails("Period")
  private val dayField = FieldDetails("Day")
  private val marketField = FieldDetails("Market")
  private val observedPeriodField = FieldDetails("Observed Period")
  private val forwardPrice = FieldDetails("Avg Forward Price")
  private val holiday = FieldDetails("Is Holiday")
  private val average = FieldDetails("ATM Swap Vol")
  private val impliedVol = FieldDetails("Implied Vol")
  private val fixed = FieldDetails("Fixed")

  private val marketDay = context.environment.marketDay
  private val startMonth = marketDay.containingMonth

  private val env = context.environment.undiscounted
  private val markets = context.marketVols.flatMap(_.market match {
    case k: CommodityMarket => Some(k)
    case _ => None
  })
  private val marketsData = context.marketVols.map(m => m.market -> m).toMap
  private val indexes = Index.all.flatMap {
    case (si: SingleIndex) if markets.contains(si.market) => Some(si)
    case _ => None
  }
  private val indexSelection = indexes.headOption.map(i => SomeSelection(Set(i.name))).getOrElse(AllSelection)

  override def initialState = {
    new PivotFieldsState(
      columns = ColumnTrees(List(
        ColumnTree(average.field, true),
        ColumnTree(marketField.field, false, List(forwardPrice, observedPeriodField, impliedVol, fixed, holiday).map(f => ColumnTree(f.field, true)) : _*))),
      rowFields = List(period, dayField).map(_.field),
      filters = List((indexField.field, indexSelection))
    )
  }

  val fieldDetailsGroups = {
    FieldDetailsGroup("Fields", indexField, period, dayField, marketField, observedPeriodField, forwardPrice, holiday, average, impliedVol, fixed) :: Nil
  }

  private def averageUnfixedPrice(index: SingleIndex, averagingPeriod: DateRange): Quantity = {
    val unfixedDays = index.observationDays(averagingPeriod).filter(_.endOfDay > marketDay)
    if (unfixedDays.isEmpty) {
      return Percentage(0)
    }
    env.averagePrice(index, DateRange(unfixedDays))
  }

  def unfilteredData(pfs: PivotFieldsState): List[Map[Field, Any]] = {
    indexes.flatMap {
      index => {
        val market = index.market
        val marketData = marketsData(market)
        val data = marketData.oilVolSurfaceData
        val months = marketDay.day.containingMonth upto data.periods.last.firstDay.containingMonth
        months.flatMap {
          case (dr) => {
            try {
              var vols = Map[Day, Percentage]()
              val F = averageUnfixedPrice(index, dr)
              val swapVol = env.swapVol(index, dr, F)
              val allFields = fields(indexField -> index.name, period -> dr, forwardPrice -> F, average -> swapVol) :: dr.toList.filter(_.isWeekday).map {
                day => {
                  fields(dayField -> day, indexField -> index.name, period -> dr, marketField -> market.name) ++
                    (if (day.endOfDay <= marketDay) {
                      fields(fixed -> true, holiday -> false)
                    } else if (index.isObservationDay(day)) {
                      val vol = env.indexVol(index, day, F, F)
                      vols += (day -> vol)
                      vol
                      val observedPeriod = index.observedOptionPeriod(day)
                      fields(fixed -> false, observedPeriodField -> observedPeriod, impliedVol -> vol, holiday -> false)
                    } else {
                      fields(fixed -> false, holiday -> true)
                    })
                }
              }
              val calculatedVol = Percentage.average(vols.values)
              assume(swapVol == calculatedVol, swapVol + "!=" + calculatedVol)
              allFields
            } catch {
              case e: AssertionError => {
                throw e
              }
              case e => {
                None
              }
            }

          }
        }
      }
    } toList
  }
}
