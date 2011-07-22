package starling.services.rpc.valuation

import org.testng.annotations.Test
import starling.market.{MarketProvider, TestMarketLookup}
import starling.services.rpc.refdata.FileMockedTitanServices
import starling.services.rabbit.MockRabbitEventServices
import com.trafigura.common.control.PipedControl._
import org.codehaus.jettison.json.JSONArray
import com.trafigura.events.{DemultiplexerClient, EventFactory, PayloadFactory}
import com.trafigura.shared.events.Event._
import org.testng.Assert._
import starling.utils.{StarlingTest, Log}
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.shared.events._
import starling.services.rpc.logistics.FileMockedTitanLogisticsServices


/**
 * Valuation service tests
 */
class ValuationServiceTest extends StarlingTest {

  class MockEventHandler(handler : (List[String]) => Unit) extends DemultiplexerClient {
    def handle(ev: Event) {
      if (ev == null) Log.warn("Got a null event")
      else {
        // Must be a starling valuation update event from valuation service
        if (StarlingSource == ev.source && StarlingValuationServiceSubject == ev.subject) {

          Log.info("handler: recieved a starling valuation service event to process %s".format(ev.toString))

          val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
          val tradeIds = tradePayloads.map(p => p.key.identifier)
          Log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))

          ev.verb match {
            case UpdatedEventVerb => {
              //updatedTradeValuationList = tradeIds
              handler(tradeIds)
            }
            case NewEventVerb => {
              Log.info("new event received")
            }
            case CancelEventVerb | RemovedEventVerb => {
              Log.info("cancel / remove event received")
            }
          }
        }
      }
    }
  }

  @Test
  def testValuationServiceValuationUpdatedEvents() {

    Log.info("testValuationServiceValuationUpdatedEvents starting")

    var updatedTradeValuationList : List[String] = Nil
    val handler = (ids : List[String]) => updatedTradeValuationList = ids 

    println("Starting valuation service tests - initialisation of mocks")

    val testMarketLookup = new TestMarketLookup()
    MarketProvider.registerNewImplForTesting(Some(testMarketLookup))
    val mockTitanServices = new FileMockedTitanServices()
    val mockTitanTradeService = new DefaultTitanTradeService(mockTitanServices)
    val mockTitanTradeCache = new TitanTradeServiceBasedTradeCache(mockTitanTradeService)
    val mockTitanLogisticsServices = FileMockedTitanLogisticsServices()
    val mockRabbitEventServices = new MockRabbitEventServices()

    val vs = new ValuationService(
      new MockEnvironmentProvider, mockTitanTradeCache, mockTitanServices, mockTitanLogisticsServices, mockRabbitEventServices)

    println("Running valuation service tests")

    //vs.marketDataSnapshotIDs().foreach(println)
    val valuations = vs.valueAllQuotas()

    val (worked, _) = valuations.tradeResults.values.partition(_ isRight)
    val valuedTradeIds = valuations.tradeResults.collect {
      case (id, Right(v)) => id
    }.toList
    val valuedTrades = vs.getTrades(valuedTradeIds)
    val firstTrade = mockTitanTradeService.getAllTrades().head // valuedTrades.head

//val x = valuedTrades.find(t => t.oid == firstTrade.oid)
//val y = valuations.tradeResults(firstTrade.oid.toString)
    
    val testEventHandler = new MockEventHandler(handler)

    mockRabbitEventServices.eventDemux.addClient(testEventHandler)

    /**
     * Test that no changes to trade value does not cause a valuation update event when a trade updated event is received
     */
    
    // publish trade updated events...
    val eventArray = createTradeUpdatedEvent(firstTrade.oid.toString)

    // publish our change event
    updatedTradeValuationList = Nil
    mockRabbitEventServices.rabbitEventPublisher.publish(eventArray)

    // check the updated valuation event is sent...
    Log.info("updatedTradeValuationList " + updatedTradeValuationList.mkString(", "))

    assertTrue(!updatedTradeValuationList.contains(firstTrade.oid.toString), "Valuation service raised valuation changed events for unchanged trades")

    /**
     * Test changing a trade value causes valuation update events for those trades
     */
    val updatedTrade = EDMPhysicalTrade.fromJson(firstTrade.toJson())
    updatedTrade.direction = if (updatedTrade.direction == "P") "S" else "P"  // make a change to cause a valuation to change value and cause a valuation updated event
    //updatedTrade.quotas.map(q => q.detail.pricingSpec.quantity.amount = Some(q.detail.pricingSpec.quantity.amount.getOrElse(0.0) + 1.0) )
    mockTitanServices.updateTrade(updatedTrade)

    // publish our change event
    updatedTradeValuationList = Nil
    mockRabbitEventServices.rabbitEventPublisher.publish(eventArray)

    // check the updated valuation event is sent...
    Log.info("updatedTradeValuationList " + updatedTradeValuationList.mkString(", "))

    assertTrue(updatedTradeValuationList.contains(updatedTrade.oid.toString), "Valuation service failed to raise valuation changed events for the changed trades")
  }


  /**
   * very basic tests to check we can value some assignments
   *   will need improvements and to also test events
   */
  @Test
  def testValuationServiceValueAssignments {
    
    Log.info("testValuationServiceValueAssignments starting")

    println("Starting valuation service assignment tests - initialisation of mocks")

    val testMarketLookup = new TestMarketLookup()
    MarketProvider.registerNewImplForTesting(Some(testMarketLookup))
    val mockTitanServices = new FileMockedTitanServices()
    val mockTitanTradeService = new DefaultTitanTradeService(mockTitanServices)
    val mockTitanTradeCache = new TitanTradeServiceBasedTradeCache(mockTitanTradeService)
    val mockTitanLogisticsServices = FileMockedTitanLogisticsServices()
    val mockRabbitEventServices = new MockRabbitEventServices()

    //val salesAssignments = mockTitanLogisticsServices.assignmentService.service.getAllSalesAssignments()
    val assignments = mockTitanLogisticsServices.assignmentService.service.getAllSalesAssignments()
    val inventory = mockTitanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
    println("assignments " + assignments.mkString(", "))
//    val inventory = mockTitanLogisticsServices.inventoryService.service.getInventoryTreeByPurchaseQuotaId()
//    println("inventory " + inventory.mkString(", "))

    val vs = new ValuationService(
      new MockEnvironmentProvider, mockTitanTradeCache, mockTitanServices, mockTitanLogisticsServices, mockRabbitEventServices)

    println("Running valuation service assignment tests")

    //vs.marketDataSnapshotIDs().foreach(println)
    val assignmentValuations = vs.valueAllAssignments()

    println("Valued assignments")

    val (worked, failed) = assignmentValuations.assignmentValuationResults.values.partition(_ isRight)
    val valuedIds = assignmentValuations.assignmentValuationResults.collect {
      case (id, Right(v)) => id
    }.toList

    println("Valued assignments, %d worked, %d failed \n%s".format(worked.size, failed.size, worked.mkString("\n")))

    assertTrue(worked.size > 0, "Assignment valuation service failed to value any assignments")
  }

  private def createTradeUpdatedEvent(id : String) : JSONArray = createTradeUpdatedEvent(List(id))
  private def createTradeUpdatedEvent(ids : List[String]) : JSONArray = {
    val pf = new PayloadFactory()
    val ef = new EventFactory()
    val source = TrademgmtSource
    val payloads = ids.map(id => pf.createPayload(RefinedMetalTradeIdPayload, source, id))
    val keyIdentifier = System.currentTimeMillis.toString
    val ev = ef.createEvent(TradeSubject, UpdatedEventVerb, source, keyIdentifier, payloads)
    ||> { new JSONArray } { r => r.put(ev.toJson) }
  }

  def main(args : Array[String]) {
    testValuationServiceValuationUpdatedEvents
  }
}

