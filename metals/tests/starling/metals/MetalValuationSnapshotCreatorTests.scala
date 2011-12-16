package starling.metals

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.quantity.UOM._
import com.trafigura.trademgmt.internal.refinedmetalfpc.MonthAveragePricingSpec
import starling.instrument.physical.{AveragePricingSpec, PhysicalMetalAssignment, PhysicalMetalQuota, PhysicalMetalForward}
import starling.market.{Level, Market, LmeCashSettlementIndex, Copper}
import starling.marketdata.{GradeCode, IncotermCode, ContractualLocationCode}
import starling.curves.{USDFXRateKey, ForwardPriceKey, DiscountRateKey, UnitTestingEnvironment}
import starling.daterange.{DayAndTime, Month, Day}

class MetalValuationSnapshotCreatorTests extends StarlingTest{

  import MetalValuationSnapshotCreator._

  val goodForward = PhysicalMetalForward(
    "id 123",
    List(
      PhysicalMetalQuota(
        quotaID = "id 123.q1",
        assignments = List(
          PhysicalMetalAssignment(
            assignmentID = "123.q1.a1",
            quantity = 100.0 (MT),
            commodity = Copper,
            contractDeliveryDay = Day(2012, 1, 20),
            contractPricingSpec = AveragePricingSpec(LmeCashSettlementIndex(Market.LME_COPPER, Level.Close), Month(2012, 1), 1.3 (USD/MT), GBP),
            contractLocationCode = ContractualLocationCode("A Place"),
            contractIncoTermCode = IncotermCode("CIF"),
            benchmarkDeliveryDay = None,
            benchmarkCountryCode = None,
            benchmarkIncoTermCode = None,
            isPurchase = true,
            inventoryID = "i1",
            inventoryQuantity = 99.0(MT),
            grade = GradeCode("grade")
          )
        ),
        unallocatedSalesPortion = None
      )
         )
       )

     val badForward = PhysicalMetalForward(
         "id 1234",
         List(
           PhysicalMetalQuota(
             quotaID = "id 1234.q1",
             assignments = List(
               PhysicalMetalAssignment(
                 assignmentID = "1234.q1.a1",
                 quantity = 100.0 (MT),
                 commodity = Copper,
                 contractDeliveryDay = Day(2012, 1, 20),
                 contractPricingSpec = new AveragePricingSpec(LmeCashSettlementIndex(Market.LME_COPPER, Level.Close), Month(2012, 1), 1.3 (USD/MT), GBP){
                   override def settlementDay(marketDay : DayAndTime) = throw new IllegalStateException("Throwing an error")
                 },
                 contractLocationCode = ContractualLocationCode("A Place"),
                 contractIncoTermCode = IncotermCode("CIF"),
                 benchmarkDeliveryDay = None,
                 benchmarkCountryCode = None,
                 benchmarkIncoTermCode = None,
                 isPurchase = true,
                 inventoryID = "i2",
                 inventoryQuantity = 99.0(MT),
                 grade = GradeCode("grade")
               )
             ),
             unallocatedSalesPortion = None
           )
    )
  )
  val marketDay = Day(2011, 12, 6).endOfDay
  val environmentWthNoMarketData = UnitTestingEnvironment(marketDay, {case _ : DiscountRateKey=> 1.0})
  val environmentWithMarketData =  UnitTestingEnvironment(marketDay,
    {
      case _ : DiscountRateKey=> 1.0 (SCALAR)
      case ForwardPriceKey(market, period, _) => 101.0 (market.priceUOM)
      case USDFXRateKey(ccy) => 1.5 (USD / ccy)
    }
  )
  @Test
  def testNoForwardsImpliesValuationUnchanged{
    var forwards =  Map[String, Either[String, PhysicalMetalForward]]()
    assertEquals(valuationChange(environmentWthNoMarketData, environmentWthNoMarketData, forwards), ValuationUnchanged)
  }

//  @Test
//  def testGoodForwardWithNoMarketData{
//    var forwards =  Map("foo" -> Right(goodForward))
//    var changeResult: ValuationChangeResult = valuationChange(environmentWthNoMarketData, environmentWthNoMarketData, forwards)
//    assertEquals(changeResult, OldAndNewValuationsNotAvailable)
//  }
//  @Test
//  def testGoodForwardWithNewMarketData{
//    var forwards =  Map("foo" -> Right(goodForward))
//    var changeResult: ValuationChangeResult = valuationChange(environmentWthNoMarketData, environmentWithMarketData, forwards)
//    assertEquals(changeResult, FirstNewValuation)
//     }
//
//  @Test
//  def testBadForwardWithNewMarketData {
//    var forwards = Map("foo" -> Right(badForward))
//    var changeResult: ValuationChangeResult = valuationChange(environmentWthNoMarketData, environmentWithMarketData, forwards)
//    assertEquals(changeResult, FirstNewValuation)
//  }

}