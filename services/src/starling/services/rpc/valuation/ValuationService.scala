package starling.services.rpc.valuation

import starling.props.Props
import starling.instrument.PhysicalMetalForward
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.gui.api.MarketDataIdentifier._
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import starling.utils.{Log, Stopwatch}
import com.trafigura.services.rabbit.{RabbitPublisher, RabbitListener, RabbitConnector}
import com.rabbitmq.client.AMQP.BasicProperties
import com.trafigura.events.{EventDemultiplexer, DemultiplexerClient}
import starling.services.{Server, StarlingInit}
import com.trafigura.services.valuation._
import com.trafigura.edm.valuation.{ValuationServiceResourceStub, ValuationService => EdmValuationService, CostsAndIncomeQuotaValuationServiceResults => EdmCostsAndIncomeQuotaValuationServiceResults}
import com.trafigura.edm.physicaltradespecs.EDMQuota
import starling.services.rpc.refdata._
import starling.edm.EDMConversions._
import com.trafigura.tradinghub.support.{ModelObject, ServiceFilter}
import com.trafigura.edm.trades.{CompletedTradeTstate, TradeTstateEnum, Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import scala.Either
import java.lang.Exception
import com.trafigura.shared.events._
import org.joda.time.{DateTime, LocalDate}
import com.trafigura.process.Pid
import java.net.InetAddress
import org.codehaus.jettison.json.JSONArray
import com.trafigura.common.control.PipedControl._
import starling.utils.cache.CacheFactory
import starling.curves.NullAtomicEnvironment
import java.io.FileWriter
import java.io.BufferedWriter
import starling.services.rabbit._
import com.trafigura.tradecapture.internal.refinedmetal.Market
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import com.trafigura.services.rabbit.Publisher
import com.trafigura.events.PayloadFactory
import com.trafigura.events.EventFactory


trait TitanTradeCache {
  protected var tradeMap: Map[String, EDMPhysicalTrade]
  protected var quotaIDToTradeIDMap: Map[String, String]
  def getTrade(id: String): EDMPhysicalTrade 
  def getAllTrades(): List[EDMPhysicalTrade] 
  def removeTrade(id : String) {
    tradeMap = tradeMap - id
    quotaIDToTradeIDMap = quotaIDToTradeIDMap.filter{ case (_, value) => value != id}
  }

  def addTrade(id : String) {
    tradeMap += id -> getTrade(id)
    addTradeQuotas(id)
  }

  def addTradeQuotas(id : String) {
    val trade = tradeMap(id)
    quotaIDToTradeIDMap ++= trade.quotas.map{quota => (quota.detail.identifier, id)}
  }

  def tradeIDFromQuotaID(quotaID: String): String 
}

case class DefaultTitanTradeCache(props : Props) extends TitanTradeCache {
  protected var tradeMap: Map[String, EDMPhysicalTrade] = Map[String, EDMPhysicalTrade]()
  protected var quotaIDToTradeIDMap: Map[String, String] = Map[String, String]()

  val titanTradesService = new DefaultTitanTacticalRefData(props).titanGetEdmTradesService

  /*
    Read all trades from Titan and blast our cache
  */
  def updateTradeMap() {
    val sw = new Stopwatch()
    val edmTradeResult = titanTradesService.getAll()
    Log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    Log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    tradeMap = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.tradeId.toString, t)).toMap
    tradeMap.keySet.foreach(addTradeQuotas)
  }

  def getAllTrades(): List[EDMPhysicalTrade] = {
    if (tradeMap.size > 0) {
      tradeMap.values.toList
    }
    else {
      updateTradeMap()
      tradeMap.values.toList
    }
  }

  def getTrade(id: String): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = titanTradesService.getByOid(id.toInt)
      tradeMap += trade.tradeId.toString -> trade.asInstanceOf[EDMPhysicalTrade]
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): String = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}

/**
 * Trade cache using supplied ref data
 */
case class RefDataTitanTradeCache(refData : TitanTacticalRefData) extends TitanTradeCache {

  protected var tradeMap: Map[String, EDMPhysicalTrade] = Map[String, EDMPhysicalTrade]()
  protected var quotaIDToTradeIDMap: Map[String, String] = Map[String, String]()

  val titanTradesService = refData.titanGetEdmTradesService

  /*
    Read all trades from Titan and blast our cache
  */
  def updateTradeMap() {
    val sw = new Stopwatch()
    val edmTradeResult = titanTradesService.getAll()
    Log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    Log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    tradeMap = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.tradeId.toString, t)).toMap
    tradeMap.keySet.foreach(addTradeQuotas)
  }

  def getAllTrades(): List[EDMPhysicalTrade] = {
    if (tradeMap.size > 0) {
      tradeMap.values.toList
    }
    else {
      updateTradeMap()
      tradeMap.values.toList
    }
  }

  def getTrade(id: String): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = titanTradesService.getByOid(id.toInt)
      tradeMap += trade.tradeId.toString -> trade.asInstanceOf[EDMPhysicalTrade]
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): String = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}

/**
 * Valuation service implementations
 */
class ValuationService(
  marketDataStore: MarketDataStore, 
  titanTradeCache : TitanTradeCache, 
  titanTacticalRefData : TitanTacticalRefData,
  rabbitEvents : RabbitEvents) extends ValuationServiceApi {

  type TradeValuationResult = Either[List[CostsAndIncomeQuotaValuation], String]

  val futuresExchangeByGUID = titanTacticalRefData.futuresExchangeByGUID
  val futuresMarketByGUID = titanTacticalRefData.futuresMarketByGUID
  val eventHandler = new EventHandler

  rabbitEvents.eventDemux.addClient(eventHandler)


  /**
   * value all edm trade quotas (that are completed) and return a structure containing a
   *   map from tradeId to either a list of CostsAndIncomeQuotaValuation or strings
   */
  def valueAllQuotas(maybeSnapshotIdentifier: Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    log("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = titanTradeCache.getAllTrades()
    log("Got Edm Trade results, trade result count = " + edmTrades.size)
    val env = environment(snapshotStringToID(snapshotIDString))
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _
    log("Got %d completed physical trades".format(edmTrades.size))
    sw.reset()
    val valuations = edmTrades.map {
      trade => (trade.tradeId.toString, tradeValuer(trade))
    }.toMap
    log("Valuation took " + sw)
    val (errors, worked) = valuations.values.partition(_ match {
      case Right(_) => true
      case Left(_) => false
    })
    log("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, valuations)
  }

  /**
   * value the quotas of a specified trade
   */
  def valueTradeQuotas(tradeId: Int, maybeSnapshotIdentifier: Option[String] = None): (String, TradeValuationResult) = {
    log("valueTradeQuotas called for trade %d with snapshot id %s".format(tradeId, maybeSnapshotIdentifier))
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)

    val sw = new Stopwatch()

    val edmTradeResult = titanTradeCache.getTrade(tradeId.toString)

    log("Got Edm Trade result " + edmTradeResult)
    val env = environment(snapshotStringToID(snapshotIDString))
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

    val edmTrade: EDMPhysicalTrade = edmTradeResult.asInstanceOf[EDMPhysicalTrade]
    log("Got %s physical trade".format(edmTrade.toString))
    sw.reset()
    val valuation = tradeValuer(edmTrade)
    log("Valuation took " + sw)

    (snapshotIDString, valuation)
  }


  /**
   * value all costables by id
   */
  def valueCostables(costableIds: List[String], maybeSnapshotIdentifier: Option[String]): CostsAndIncomeQuotaValuationServiceResults = {
    
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environment(snapshotStringToID(snapshotIDString))
    valueCostables(costableIds, env, snapshotIDString)
  }
  
  def valueCostables(costableIds: List[String], env : Environment, snapshotIDString : String): CostsAndIncomeQuotaValuationServiceResults = {

    val sw = new Stopwatch()

    val idsToUse = costableIds match {
      case Nil | null => titanTradeCache.getAllTrades().map{trade => trade.tradeId.toString}
      case list => list
    }

    var tradeValueCache = Map[String, TradeValuationResult]()
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

    def tradeValue(id: String): TradeValuationResult = {
      if (!tradeValueCache.contains(id))
        tradeValueCache += (id -> tradeValuer(titanTradeCache.getTrade(id)))
      tradeValueCache(id)
    }

    def quotaValue(id: String) = {
      tradeValue(titanTradeCache.tradeIDFromQuotaID(id)) match {
        case Left(list) => Left(list.filter(_ .quotaID == id))
        case other => other
      }
    }

    val (tradeIDs, quotaIDs) = idsToUse.span {
      id => try {
        Integer.parseInt(id); true
      } catch {
        case _: NumberFormatException => false
      }
    }

    val tradeValues = tradeIDs.map { case id => (id, tradeValue(id)) }
    val quotaValues = quotaIDs.map { case id => (id, quotaValue(id)) }

    val valuations = tradeValues ::: quotaValues

    log("Valuation took " + sw)
    val (errors, worked) = valuations.partition(_._2 match {
      case Right(_) => true
      case Left(_) => false
    })
    log("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)

    CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * Return all snapshots for a given observation day, or every snapshot if no day is supplied
   */
  def marketDataSnapshotIDs(observationDay: Option[LocalDate] = None): List[TitanSnapshotIdentifier] = {
    updateSnapshotCache()
    snapshotNameToID.values.filter {
      starlingSnapshotID =>
        starlingSnapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) && (observationDay.isEmpty || (starlingSnapshotID.observationDay.toJodaLocalDate == observationDay.get))
    }.map {
      starlingSnapshotID => TitanSnapshotIdentifier(starlingSnapshotID.id.toString, starlingSnapshotID.observationDay.toJodaLocalDate)
    }.toList
  }

  private def log(msg: String) = Log.info("ValuationService: " + msg)

  private def resolveSnapshotIdString(maybeSnapshotIdentifier: Option[String] = None) = {
    val snapshotIDString = maybeSnapshotIdentifier.orElse(mostRecentSnapshotIdentifierBeforeToday()) match {
      case Some(id) => id
      case _ => throw new IllegalStateException("No market data snapshots")
    }
    log("Actual snapshot ID " + snapshotIDString)
    snapshotIDString
  }

  // snapshot helpers...
  private val snapshotNameToID = scala.collection.mutable.Map[String, SnapshotID]()
  private val lock = new Object()

  private def updateSnapshotCache() {
    lock.synchronized {
      marketDataStore.snapshots().foreach {
        s: SnapshotID =>
          snapshotNameToID += s.id.toString -> s
      }
    }
  }

  private def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = {
    updateSnapshotCache()
    snapshotNameToID.values.toList.filter(_.observationDay < Day.today()).sortWith(_ > _).headOption.map(_.id.toString)
  }

  private def snapshotStringToID(id: String): SnapshotID = {
    snapshotNameToID.getOrElse(id, {
      updateSnapshotCache()
      assert(snapshotNameToID.contains(id), "Snapshot ID " + id + " not found")
      snapshotNameToID(id)
    })
  }

  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)
  private def environment(snapshotID: SnapshotID): Environment = environmentCache.memoize(
    snapshotID,
    {snapshotID : SnapshotID => {
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshotID.marketDataSelection, snapshotID.version))
      ClosesEnvironmentRule.createEnv(snapshotID.observationDay, reader).environment
    }}
  )

  def getTrades(tradeIds : List[String]) : List[EDMPhysicalTrade] = tradeIds.map(titanTradeCache.getTrade)
  def getFuturesExchanges = futuresExchangeByGUID.values
  def getFuturesMarkets = futuresMarketByGUID.values

  /**
   * handler for events
   */
  class EventHandler extends DemultiplexerClient {
    val rabbitPublishChangedValueEvents = publishChangedValueEvents(rabbitEvents.rabbitEventPublisher) _
    def handle(ev: Event) {
      if (ev == null) Log.warn("Got a null event") else {
        if (Event.TrademgmtSource == ev.source && Event.TradeSubject == ev.subject) { // Must be a trade event from trademgmt
          Log.info("handler: Got a trade event to process %s".format(ev.toString))

          val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
          val tradeIds = tradePayloads.map(p => p.key.identifier)
          Log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))

          ev.verb match {
            case UpdatedEventVerb => {
              val (snapshotIDString, env) = mostRecentSnapshotIdentifierBeforeToday() match {
                case Some(snapshotId) => (snapshotId, environment(snapshotStringToID(snapshotId)))
                case None => ("No Snapshot found",  Environment(NullAtomicEnvironment((Day.today() - 1).startOfDay)))
              }

              val originalTradeValuations = valueCostables(tradeIds, env, snapshotIDString)
              println("originalTradeValuations = " + originalTradeValuations)
              tradeIds.foreach{ id => titanTradeCache.removeTrade(id); titanTradeCache.addTrade(id)}
              val newTradeValuations = valueCostables(tradeIds, env, snapshotIDString)
              val changedIDs = tradeIds.filter{id => newTradeValuations.tradeResults(id) != originalTradeValuations.tradeResults(id)}

              if (changedIDs != Nil)
                rabbitPublishChangedValueEvents(changedIDs)

              Log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
            }
            case NewEventVerb => {
              tradeIds.foreach(titanTradeCache.addTrade)
              Log.info("New event received for %s".format(tradeIds))
            }
            case CancelEventVerb | RemovedEventVerb => {
              tradeIds.foreach(titanTradeCache.removeTrade)
              Log.info("Cancelled / deleted event received for %s".format(tradeIds))
            }
          }
        }
      }
    }

    // publish the valuation updated event contaning payloads of the trade id's whose trade valuations have changed
    def publishChangedValueEvents(eventPublisher : Publisher)(tradeIds : List[String]) = {
      val newValuationEvent =
        new Event() {
          verb = UpdatedEventVerb
          subject = Event.StarlingValuationServiceSubject
          source = Event.StarlingSource
          content = new Content() {
            header = new Header() {
              timestamp = new DateTime
              pid = Pid.getPid
              host = InetAddress.getLocalHost.getCanonicalHostName
            }
            body = Body(tradeIds.map(id => new Payload() {
              payloadType = Event.RefinedMetalTradeIdPayload
              key = new EventKey() { identifier = id }
              source = Event.StarlingSource
            }))
            key = new EventKey(){identifier = System.currentTimeMillis.toString}
          }
        }

      val eventArray = ||> { new JSONArray } { r => r.put(newValuationEvent.toJson) }
      eventPublisher.publish(eventArray)
    }
  }
}


/**
 * Titan EDM model exposed services wrappers
 */
class ValuationServiceResourceStubEx
  extends ValuationServiceResourceStub(new ValuationServiceRpc(Server.server.marketDataStore, Server.server.valuationService), new java.util.ArrayList[ServiceFilter]()) {

  override def requireFilters(filterClasses: String*) {}
}

class ValuationServiceRpc(marketDataStore: MarketDataStore, valuationService: ValuationService) extends EdmValuationService {

  def valueAllQuotas(maybeSnapshotIdentifier: String): EdmCostsAndIncomeQuotaValuationServiceResults = {

    Log.info("ValuationServiceRpc valueAllQuotas %s".format(maybeSnapshotIdentifier))

    val valuationResult = valuationService.valueAllQuotas(Option(maybeSnapshotIdentifier))

    Log.info("got valuationResult, size %d".format(valuationResult.tradeResults.size))

    valuationResult
  }

  def valueCostables(costableIds: List[String], maybeSnapshotIdentifier: String): EdmCostsAndIncomeQuotaValuationServiceResults = {

    valuationService.valueCostables(costableIds, Option(maybeSnapshotIdentifier))
  }
}


object ValuationService extends Application {

  import org.codehaus.jettison.json.JSONObject

  println("Here")
  lazy val vs = StarlingInit.devInstance.valuationService

  vs.marketDataSnapshotIDs().foreach(println)
  val valuations = vs.valueAllQuotas()

//  valuations.tradeResults.foreach(println)
   
  val (_, worked) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })

  val valuedTradeIds = valuations.tradeResults.collect{ case (id, Left(v)) => id }.toList
  val valuedTrades = vs.getTrades(valuedTradeIds)
  val markets = vs.getFuturesMarkets.toList
  val exchanges = vs.getFuturesExchanges.toList

  val tradesFile = "/tmp/edmTrades.json"
  val marketsFile = "/tmp/markets.json"
  val exchangesFile = "/tmp/exchanges.json"

  writeJson(tradesFile, valuedTrades)
  writeJson(marketsFile, markets)
  writeJson(exchangesFile, exchanges)

  val loadedMarkets = loadJsonValuesFromFile(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
  val loadedTrades = loadJsonValuesFromFile(tradesFile).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])

  loadedMarkets.foreach(println)
  loadedExchanges.foreach(println)
  println("loaded trade size = " + loadedTrades.size)
  
  StarlingInit.devInstance.stop

  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T]) {
    try {
      val fStream = new FileWriter(fileName)
      val bWriter = new BufferedWriter(fStream)
      objects.foreach(obj => bWriter.write(obj.toJson().toString() + "\n" ))
      bWriter.flush()
      fStream.close()
    }
    catch {
      case ex : Exception => println("Error: " + ex.getMessage())
    }
  }

  import scala.io.Source._
  def loadJsonValuesFromFile(fileName : String) : List[String] = 
    fromFile(fileName).getLines.toList

//  def fromJson[T <: ModelObject with Object { def fromJson() : JSONObject }](json : List[String]) = {
//    json.map(s => fromJson(s).asInstanceOf[T])
//  }
}


/**
 * Valuation service tests
 */
object ValuationServiceTest extends Application {
  import Event._
  println("Starting valuation service tests")
  lazy val server = StarlingInit.devInstance
  val mockTitanTacticalRefData = new FileMockedTitanTacticalRefData()
  val mockTitanTradeCache = new RefDataTitanTradeCache(mockTitanTacticalRefData)
  val mockRabbitEvents = new MockRabbitEvents()

  val vs = new ValuationService(
    server.marketDataStore,  mockTitanTradeCache, mockTitanTacticalRefData, mockRabbitEvents)

  // publish trade updated events...
  val pf = new PayloadFactory()
  val ef = new EventFactory()
  val source = TrademgmtSource
  val keyId = "1" // trade Id
  val payloads = List(pf.createPayload(RefinedMetalTradeIdPayload, source, keyId))
  val keyIdentifier = System.currentTimeMillis.toString
  val ev = ef.createEvent(TradeSubject, UpdatedEventVerb, source, keyIdentifier, payloads)
  val eventArray = ||> { new JSONArray } { r => r.put(ev.toJson) }
  mockRabbitEvents.rabbitEventPublisher.publish(eventArray)

  vs.marketDataSnapshotIDs().foreach(println)
  val valuations = vs.valueAllQuotas()
   
  val (_, worked) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })

  val valuedTradeIds = valuations.tradeResults.collect{ case (id, Left(v)) => id }.toList

  val valuedTrades = vs.getTrades(valuedTradeIds)

  StarlingInit.devInstance.stop
}

