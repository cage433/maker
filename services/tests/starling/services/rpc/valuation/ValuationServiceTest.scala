package starling.services.rpc.valuation

import org.testng.annotations.Test
import starling.market.{MarketProvider, TestMarketLookup}
import starling.services.rpc.refdata.FileMockedTitanServices
import starling.services.rabbit.MockRabbitEventServices
import com.trafigura.edm.trades.PhysicalTrade
import com.trafigura.common.control.PipedControl._
import org.codehaus.jettison.json.JSONArray
import com.trafigura.events.{DemultiplexerClient, EventFactory, PayloadFactory}
import com.trafigura.shared.events._
import com.trafigura.shared.events.Event._
import org.testng.Assert._
import starling.utils.{StarlingTest, Log}
import com.trafigura.edm.trades.{CompletedTradeTstate, TradeTstateEnum, Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.shared.events._
import starling.services.rpc.logistics.{FileMockedTitanLogisticsServices}

/**
 * Valuation service tests
 */
class ValuationServiceTest extends StarlingTest {

  @Test
  def testValuationServiceValuationUpdatedEvents() {

    Log.info("testValuationServiceValuationUpdatedEvents starting")

    var updatedTradeValuationList : List[String] = List()

    class MockEventHandler extends DemultiplexerClient {
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
                updatedTradeValuationList = tradeIds
              }
              case CreatedEventVerb => {
                Log.info("new event received")
              }
              case CancelledEventVerb | RemovedEventVerb => {
                Log.info("cancel / remove event received")
              }
            }
          }
        }
      }
    }

    println("Starting valuation service tests - initialisation of mocks")

    val testMarketLookup = new TestMarketLookup()
    MarketProvider.registerNewImplForTesting(Some(testMarketLookup))
    val mockTitanServices = new FileMockedTitanServices()
    val mockTitanTradeService = new DefaultTitanTradeService(mockTitanServices)
    val mockTitanTradeCache = new TitanTradeServiceBasedTradeCache(mockTitanTradeService)
    val mockTitanLogisticsServices = FileMockedTitanLogisticsServices()
    val assignments = mockTitanLogisticsServices.service.getAllSalesAssignments()
    val mockRabbitEventServices = new MockRabbitEventServices()

assignments.foreach(println)

    val vs = new ValuationService(
      new MockEnvironmentProvider, mockTitanTradeCache, mockTitanServices, mockTitanLogisticsServices, mockRabbitEventServices)

    println("Running valuation service tests")

    //vs.marketDataSnapshotIDs().foreach(println)
    val valuations = vs.valueAllQuotas()

    val (_, worked) = valuations.tradeResults.values.partition({
      case Right(_) => true;
      case _ => false
    })
    val valuedTradeIds = valuations.tradeResults.collect {
      case (id, Left(v)) => id
    }.toList
    val valuedTrades = vs.getTrades(valuedTradeIds)
    val firstTrade = mockTitanTradeService.getAllTrades().head //valuedTrades.head

//val x = valuedTrades.find(t => t.oid == firstTrade.oid)
//val y = valuations.tradeResults(firstTrade.oid.toString)
    
    val testEventHandler = new MockEventHandler

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
    mockTitanServices.updateTrade(updatedTrade)

    // publish our change event
    updatedTradeValuationList = Nil
    mockRabbitEventServices.rabbitEventPublisher.publish(eventArray)

    // check the updated valuation event is sent...
    Log.info("updatedTradeValuationList " + updatedTradeValuationList.mkString(", "))

    assertTrue(updatedTradeValuationList.contains(updatedTrade.oid.toString), "Valuation service failed to raise valuation changed events for the changed trades")
  }

  private def createTradeUpdatedEvent(id : String) = {
    val pf = new PayloadFactory()
    val ef = new EventFactory()
    val source = TrademgmtSource
    val keyId = id
    val payloads = List(pf.createPayload(RefinedMetalTradeIdPayload, source, keyId))
    val keyIdentifier = System.currentTimeMillis.toString
    val ev = ef.createEvent(TradeSubject, UpdatedEventVerb, source, keyIdentifier, payloads)
    ||> { new JSONArray } { r => r.put(ev.toJson) }
  }

  def main(args : Array[String]) {
    testValuationServiceValuationUpdatedEvents
  }
}
