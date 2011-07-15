package starling.services.trinity

import org.scalatest.matchers.ShouldMatchers
import starling.pivot.controller.PivotGrid
import starling.utils.ImplicitConversions._
import starling.daterange.Day
import starling.daterange.Day._
import starling.market.{FuturesMarket, Market}
import starling.pivot.model.{ValueAxisValueType, AxisValue, AxisCell}
import starling.pivot.{PivotQuantity, TableCell, Field}
import starling.quantity.UOM
import starling.utils.StarlingTest
import org.testng.annotations.Test

class FCLGeneratorTests extends StarlingTest with ShouldMatchers {
  @Test
  def shouldGenerateFile {
    val period: Day = 12 Mar 2045
    val price: Double = 123.456
    val market = Market.LME_COPPER

    val grid = new PivotGrid(rowData = Array(Array(cell(market.name), cell(period))),
                             colData = Array(Array()),
                             mainData = Array(Array(TableCell(new PivotQuantity(price, UOM.GBP)))))

    val expectedTrinityCode = "XXX"

    FCLGenerator.generateLines( (m:String)=> if (m==market.name) expectedTrinityCode else "e", grid) should be ===
      List("3300C%s     %sFF%s0000000000000000000000000000000CN" %
        (expectedTrinityCode, period.toString("YYMMdd"), "0000123.4560"))
  }

  @Test
  def shouldGenerateFile2 {
    val period: Day = 12 Mar 2045
    val price: Double = 88.5
    val market = Market.LME_COPPER

    val grid = new PivotGrid(rowData = Array(Array(cell(market.name), cell(period))),
                             colData = Array(Array()),
                             mainData = Array(Array(TableCell(new PivotQuantity(price, UOM.GBP)))))

    val expectedTrinityCode = "XXX"

    FCLGenerator.generateLines((m:String)=> if (m==market.name) expectedTrinityCode else "e", grid) should be ===
      List("3300C%s     %sFF%s0000000000000000000000000000000CN" %
        (expectedTrinityCode, period.toString("YYMMdd"), "0000088.5000"))
  }

  def cell(value: Any) = AxisCell.Null.copy(value = AxisValue(Field(""), ValueAxisValueType(value), 0))
}