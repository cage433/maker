package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.marketdata._
import starling.quantity.utils.QuantityTestUtils._
import starling.daterange.{Month, Tenor, Day}
import starling.market.FuturesExchangeFactory

class CountryBenchmarkCurveObjectTests extends StarlingTest{
  
  val ctryCode = NeptuneCountryCode("foo")
  val gradeCode = GradeCode("grr")
  val refData = new ReferenceDataLookup {
    def name = "fred"
    val areas = Map[AreaCode, Area]()
    val contractLocations = Map[ContractualLocationCode, ContractualLocation]()
    val grades = Map(gradeCode -> Grade(gradeCode, "grr"))
    val countries = Map(ctryCode -> NeptuneCountry(ctryCode, "foo", None))
    val incoterms = Map[IncotermCode, Incoterm]()
  }

  var currentMonth = Tenor(Month, 0)
  var nextMonth = Tenor(Month, 1)

  @Test
  def testMonthOffsetsFollowShanghiaFuturesExpiry {

    val curve = CountryBenchmarkCurveObject(
      Day(2011, 12, 1).endOfDay,
      CountryBenchmarkData(Map((ctryCode, gradeCode) -> Map(
        currentMonth -> Quantity(100, USD/MT),
        nextMonth -> Quantity(90, USD/MT)))),
      refData
    )
    assertQtyEquals(curve((ctryCode, gradeCode, currentMonth)), 100.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, nextMonth)),  90.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, Tenor(Month, 12))),  90.0 (USD/MT), 1e-6)

  }
}