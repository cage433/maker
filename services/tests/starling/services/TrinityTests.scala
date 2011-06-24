package starling.services

import org.scalatest.matchers.ShouldMatchers
import starling.pivot.controller.PivotGrid
import starling.utils.ImplicitConversions._
import starling.daterange.Day._
import starling.pivot.model.{ValueAxisValueType, AxisValue, AxisCell}
import starling.pivot.{PivotQuantity, TableCell, Field}
import starling.quantity.UOM
import starling.utils.StarlingTest
import org.testng.annotations.Test
import collection.immutable.List
import java.lang.String
import starling.market.{TrinityMarket, Market}


class TrinityTests extends StarlingTest with ShouldMatchers {
  val period = 12 Mar 2045
  val market = Market.LME_COPPER
  val trinityCode = TrinityMarket.marketToTrinityCode(market)

  @Test
  def shouldGenerateFuturesPricesExportFile {
    Trinity.generateFuturesPricesExportFile(grid(123.456)) should be === expectedFile("0000123.4560")
    Trinity.generateFuturesPricesExportFile(grid(88.5)) should be === expectedFile("0000088.5000")
  }

  def grid(price: Double) = new PivotGrid(rowData = Array(Array(cell(market.name), cell(period))),
                                          colData = Array(Array()),
                                          mainData = Array(Array(TableCell(new PivotQuantity(price, UOM.GBP)))))

  def cell(value: Any) = AxisCell.Null.copy(value = AxisValue(Field(""), ValueAxisValueType(value), 0))

  def expectedFile(price: String) =
    List("3300C%s     %sFF%s0000000000000000000000000000000CN" % (trinityCode, period.toString("YYMMdd"), price))
}