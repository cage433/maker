package starling.curves

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.daterange.Day
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import starling.marketdata._
import starling.quantity.utils.QuantityTestUtils._

class CountryBenchmarkCurveObjectTests extends StarlingTest{
  
  @Test
  def testWeDontExtrapolateBeforeFirstEffectiveDate{

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
    var firstEffectiveDay: Day = Day(2011, 12, 30)
    var secondEffectiveDay: Day = Day(2012, 1, 20)
    val curve = CountryBenchmarkCurveObject(
      Day(2011, 11, 28).endOfDay,
      CountryBenchmarkData(Map((ctryCode, gradeCode) -> Map(firstEffectiveDay -> Quantity(100, USD/MT), secondEffectiveDay -> Quantity(90, USD/MT)))),
      refData
    )
    assertQtyEquals(curve((ctryCode, gradeCode, firstEffectiveDay)), 100.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, firstEffectiveDay + 1)), 100.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, secondEffectiveDay - 1)), 100.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, secondEffectiveDay)), 90.0 (USD/MT), 1e-6)
    assertQtyEquals(curve((ctryCode, gradeCode, secondEffectiveDay + 1)), 90.0 (USD/MT), 1e-6)

    try {
      curve((ctryCode, gradeCode, firstEffectiveDay - 1))
    } catch {
      case _ : MissingMarketDataException => 
    }
  }

}