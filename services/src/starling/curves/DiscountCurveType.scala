package starling.curves

import starling.db.MarketDataReader
import starling.pivot._
import starling.quantity.{Percentage, UOM, Quantity}

object DiscountCurveType extends CurveType {
  def create(reader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, reader)

    new DiscountCurvePivotTableDataSource(context)
  }
}

class DiscountCurvePivotTableDataSource(context:EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {

  val currencyField = FieldDetails("Currency")
  val dayField = FieldDetails("Day")
  val rateField = FieldDetails.createMeasure("Rate")
  val discountField = new PivotQuantityFieldDetails("Discount")

  def fieldDetailsGroups = List(FieldDetailsGroup("Discount", currencyField, dayField, rateField, discountField))

  val allRows: List[Map[Field, Any]] = {
    val marketDay = context.environment.marketDay.day
    val env = context.environment
    context.discounts.flatMap {
      case (currency, forwardRateData) => {
        val lastDay = forwardRateData.lastDay
        val nextMonth = marketDay.containingMonth.next
        val lastMonth = lastDay.containingMonth
        val days = (nextMonth upto lastMonth).toList.map { m => m.firstDay}
        days.map {
          day => {
            val (discount, rate) = {
              try {
                (PivotQuantity(env.discount(currency, day)),env.zeroRate(currency, day))
              } catch {
                case e:Exception => (new PivotQuantity(e), new Percentage(0))
              }
            }

            fields( currencyField -> currency, dayField -> day, rateField -> rate, discountField -> discount)
          }
        }
      }
    }
  }

  def unfilteredData(pfs: PivotFieldsState) = allRows
}
