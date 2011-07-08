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

    val testMarketLookup = new TestMarketLookup()
    MarketProvider.registerNewImplForTesting(Some(testMarketLookup))
    println("Starting valuation service tests")
    val mockTitanServices = new FileMockedTitanServices()
    val mockTitanTradeService = new DefaultTitanTradeService(mockTitanServices)
    val mockTitanTradeCache = new RefDataTitanTradeCache(mockTitanServices, mockTitanTradeService)
    val mockRabbitEventServices = new MockRabbitEventServices()

    val vs = new ValuationService(
      new MockEnvironmentProvider, mockTitanTradeCache, mockTitanServices, mockRabbitEventServices)

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

    val firstTrade = mockTitanTradeService.getAllTrades().head
    val updatedTrade = EDMPhysicalTrade.fromJson(firstTrade.toJson())
    updatedTrade.direction = if (updatedTrade.direction == "P") "S" else "P"  // make a change to cause a valuation to change value and cause a valuation updated event
    mockTitanServices.updateTrade(updatedTrade)

    // publish trade updated events...
    val pf = new PayloadFactory()
    val ef = new EventFactory()
    val source = TrademgmtSource
    val keyId = updatedTrade.oid.toString
    val payloads = List(pf.createPayload(RefinedMetalTradeIdPayload, source, keyId))
    val keyIdentifier = System.currentTimeMillis.toString
    val ev = ef.createEvent(TradeSubject, UpdatedEventVerb, source, keyIdentifier, payloads)
    val eventArray = ||> { new JSONArray } { r => r.put(ev.toJson) }

    val testEventHandler = new MockEventHandler

    mockRabbitEventServices.eventDemux.addClient(testEventHandler)

    // publish our change event
    mockRabbitEventServices.rabbitEventPublisher.publish(eventArray)

    // check the updated valuation event is sent...
    Log.info("updatedTradeValuationList " + updatedTradeValuationList.mkString(", "))

    assertTrue(updatedTradeValuationList.contains(updatedTrade.oid.toString), "Valuation service failed to raise valuation changed events for the changed trades")
  }

  def main(args : Array[String]) {
    testValuationServiceValuationUpdatedEvents
  }
}
