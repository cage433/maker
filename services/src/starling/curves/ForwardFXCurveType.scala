package starling.curves

import starling.db.MarketDataReader
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.quantity.UOM

object ForwardFXCurveType extends CurveType {
  def create(reader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val context = envSpec.environmentRule.createEnv(envSpec.observationDay, reader)
    new ForwardFXCurveTypePivotTableDataSource(context)
  }
}

class ForwardFXCurveTypePivotTableDataSource(context:EnvironmentWithDomain) extends UnfilteredPivotTableDataSource {
  val cc1Field = FieldDetails("CCY1")
  val cc2Field = FieldDetails("per CCY2")
  val dayField = FieldDetails("Day")
  val rateField = new PivotQuantityFieldDetails("Rate")
  val fieldDetailsGroups = List(FieldDetailsGroup("Forward FX", List(cc1Field, cc2Field, dayField, rateField)))

  val allRows = {
    val currencies = {
      val discountCurrencies = context.discounts.toMap.mapValues(_.lastDay)
      val spotFXCurrencies = context.spotFX.toSet
      (discountCurrencies.filterKeys(ccy => ccy == UOM.USD || spotFXCurrencies.contains(ccy))).toList.sortWith(_._1 < _._1)
    }
    val env = context.environment
    val marketDay = env.marketDay.day
    val nextMonth = marketDay.containingMonth.next
    currencies.flatMap { case (ccA, ccALastDay) => {
      currencies.flatMap { case(ccB,ccBLastDay) => {
        if (ccA == ccB) {
          Nil
        } else {
          val lastDay = ccALastDay min ccBLastDay
          val lastMonth = lastDay.containingMonth
          (nextMonth upto lastMonth).map { month => {
            val day = month.lastDay
            val rate = PivotQuantity.calcOrCatch(env.forwardFXRate(ccA, ccB, day))
            fields(cc1Field -> ccA, cc2Field -> ccB, dayField -> day, rateField -> rate)
          }}
        }
      }
      }
    }

    }
  }

  def unfilteredData(pfs: PivotFieldsState) = allRows
}