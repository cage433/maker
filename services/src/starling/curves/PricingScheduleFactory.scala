package starling.curves

import starling.db.MarketDataReader
import starling.reports.pivot.IndexRuleEvaluation
import starling.quantity.UOM._
import starling.pivot._
import starling.market.rules.{SwapPricingRule, NonCommonPricingRule, Precision}
import starling.utils.{AppendingMap, Log, Stopwatch}
import starling.daterange.{Day, DateRange, Month}
import starling.market.{CommodityMarket, Index}
import starling.utils.cache.CacheFactory
import starling.market.formula.FormulaIndex

object PricingScheduleFactory extends CurveType {
  def create(marketDataReader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, marketDataReader)

    new PricingSchedulePivotDataSource(context)
  }
}

class PricingSchedulePivotDataSource(context: EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {
  private val indexRuleEvaluationCache = CacheFactory.getCache("IndexRuleEvaluation")

  val payoffRule = FieldDetails("Payoff Rule")
  val swapRule = FieldDetails("Swap Rule")
  val rounding = FieldDetails("Rounding")
  val period = FieldDetails("Period")
  val day = FieldDetails("Day")
  val market = FieldDetails("Market")
  val observedPeriod = FieldDetails("Observed Period")
  val price = FieldDetails("Price")
  val priceInRuleUOM = FieldDetails("Converted Price")
  val forward = FieldDetails("Is Forward")
  val holiday = FieldDetails("Is Holiday")
  val average = FieldDetails("Average")

  val marketDay = context.environment.marketDay
  val startMonth = marketDay.day.startOfFinancialYear.containingMonth

  val marketsWithForwardData = context.markets.map(_.market).toSet

  private val data = {
    val marketDay = context.environment.marketDay

    val indexes = Index.all.filter {
      case fi: FormulaIndex => fi.isValid
      case _ => true
    }.filter {
      Index.markets(_).forall(marketsWithForwardData.contains(_))
    }

    indexes.flatMap(index => {
      Index.markets(index).flatMap {
        commMarket =>
          precisions(Some(index)).flatMap {
            precision =>
              index.possiblePricingRules.flatMap {
                pricingRule =>
                  (monthsFor(index)).flatMap {
                    month => {
                      //
                      new AppendingMap(Map("a" -> fields(rounding -> precisionToString(precision), swapRule -> pricingRule.name, payoffRule -> index.name, period -> month),
                        "b" -> lazyAverage(index, month, pricingRule, precision, context.environment)
                      )) ::
                        month.toList.filter(_.isWeekday).map {
                          obDay => {
                            val isForward = obDay.endOfDay > marketDay
                            new AppendingMap(Map("a" -> fields(
                              rounding -> precisionToString(precision),
                              swapRule -> pricingRule.name, payoffRule -> index.name, period -> month,
                              day -> obDay, market -> commMarket.name, forward -> isForward),
                              "b" -> lazyMap(index, month, pricingRule, precision, context.environment, obDay, commMarket)
                            ))
                          }
                        }
                    }
                  }
              }
          }
      }
    })
  }

  def initialIndex = data.headOption.map(e => Index.fromName(e(payoffRule.field).toString))

  private def precisions(index: Option[Index]) = index.map(_.precision match {
    case Some(p) => List(Some(p.default), Some(p.clearPort))
    case None => List(None)
  }).getOrElse(List(None))

  val fieldDetailsGroups = {
    FieldDetailsGroup("Fields", payoffRule, swapRule, rounding, period, day, market, observedPeriod, price, priceInRuleUOM, forward, holiday, average) :: Nil
  }

  def monthsFor(index: Index) = {
    val lastForwardPeriod = Index.markets(index).map {
      market => context.markets.find(_.market == market).map(
        marketWithDeliveryPeriod => (marketWithDeliveryPeriod.periods.last)
      )
    } min

    lastForwardPeriod.map(lastPeriod => {
      val lastAveragingPeriod = index.indexes.map(i => {
        val days = i.observationDays(context.environment.marketDay.day upto lastPeriod.lastDay)
        days.filter(day => i.observedPeriod(day) <= lastPeriod) max
      }
      ).max.containingMonth
      startMonth upto lastAveragingPeriod
    }).getOrElse(List())
  }

  val periodSelection = initialIndex.map(i => (monthsFor(i) intersect (marketDay.containingMonth upto (marketDay.containingMonth + 12)))).getOrElse(Nil)

  override def initialState = {
    new PivotFieldsState(
      columns = ColumnTrees(
        List(ColumnTree(average.field, true),
          ColumnTree(market.field, false, List(price, observedPeriod, holiday).map(f => ColumnTree(f.field, true)) : _*))),
      rowFields = List(period, day, forward).map(_.field),
      filters = List(
        payoffRule.field -> initialIndex.map(i => SomeSelection(Set(i.name))).getOrElse(AllSelection),
        swapRule.field -> initialIndex.map(i => SomeSelection(Set(i.possiblePricingRules.head.name))).getOrElse(AllSelection),
        rounding.field -> SomeSelection(Set(precisionToString(precisions(initialIndex).head))),
        period.field -> SomeSelection(periodSelection.toSet))
    )
  }

  private def precisionToString(dp: Option[Int]) = dp.map(_ + " dp").getOrElse("None")

  def unfilteredData(pfs: PivotFieldsState): List[Map[Field, Any]] = {
    data
  }

  def lazyAverage(index: Index, period: DateRange, pricingRule: SwapPricingRule, rounding: Option[Int], environment: Environment) = {
    lazy val (rows, avg) = indexRuleEvaluationCache.memoize((index, period, pricingRule, rounding, context.environment), IndexRuleEvaluation.rows(index, period, pricingRule, rounding, context.environment))

    new scala.collection.immutable.Map[Field, Any]() {
      lazy val map = {
        fields(average -> avg)
      }

      def +[B1 >: Any](kv: (Field, B1)) = throw new Exception("Not used")

      def -(key: Field) = throw new Exception("Not used")

      def iterator = map.iterator

      def get(key: Field) = map.get(key)
    }
  }

  def lazyMap(index: Index, period: DateRange, pricingRule: SwapPricingRule, rounding: Option[Int],
              environment: Environment, obDay: Day, commMarket: CommodityMarket) = {
    lazy val (rows, avg) = indexRuleEvaluationCache.memoize((index, period, pricingRule, rounding, context.environment), IndexRuleEvaluation.rows(index, period, pricingRule, rounding, context.environment))

    new scala.collection.immutable.Map[Field, Any]() {
      lazy val map = {
        val isHoliday = !commMarket.isObservationDay(obDay)
        rows.find(r => r.day == obDay && r.market == commMarket) match {
          case Some(row) => {
            fields(
              observedPeriod -> row.observed,
              price -> row.value,
              priceInRuleUOM -> row.valueInIndexUOM,
              holiday -> isHoliday
            )
          }
          case None => {
            fields(
              holiday -> isHoliday
            )
          }
        }

      }

      def +[B1 >: Any](kv: (Field, B1)) = throw new Exception("Not used")

      def -(key: Field) = throw new Exception("Not used")

      def iterator = map.iterator

      def get(key: Field) = map.get(key)
    }


  }
}