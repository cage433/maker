package starling.services.rpc.valuation

import starling.props.Props
import starling.instrument.PhysicalMetalAssignmentForward
import starling.instrument.PhysicalMetalForward
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import starling.utils.{Log, Stopwatch}
import com.trafigura.events.DemultiplexerClient
import starling.services.StarlingInit
import com.trafigura.services.valuation._
import starling.services.rpc.refdata._
import com.trafigura.tradinghub.support.ModelObject
import com.trafigura.edm.trades.{Trade => EDMTrade, PhysicalTrade => EDMPhysicalTrade}
import com.trafigura.edm.logistics.inventory.EDMAssignmentItem
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
import starling.quantity.Quantity
import starling.curves.DiscountRateKey
import starling.curves.ForwardPriceKey
import starling.curves.UnitTestingAtomicEnvironment
import starling.curves.FixingKey
import starling.services.rpc.logistics.TitanLogisticsServices
import com.trafigura.edm.physicaltradespecs.EDMQuota


/**
 * Trade cache provide trade map lookup by trade id and also a quota id to trade map lookup
 */
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

  val titanTradesService = new DefaultTitanServices(props).titanGetEdmTradesService

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

trait TitanTradeService {
  def getTrade(id : String) : EDMPhysicalTrade
  def getAllTrades() : List[EDMPhysicalTrade]
}

class DefaultTitanTradeService(titanServices : TitanServices) extends TitanTradeService {

  def getTrade(id : String) : EDMPhysicalTrade = {
    titanServices.titanGetEdmTradesService.getByOid(id.toInt).asInstanceOf[EDMPhysicalTrade]
  }

  def getAllTrades() : List[EDMPhysicalTrade] = {
    val sw = new Stopwatch()
    val edmTradeResult = titanServices.titanGetEdmTradesService.getAll()
    Log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    Log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
  }
}

/**
 * Trade cache using supplied ref data
 */
case class TitanTradeServiceBasedTradeCache(titanTradesService : TitanTradeService) extends TitanTradeCache {

  protected var tradeMap: Map[String, EDMPhysicalTrade] = Map[String, EDMPhysicalTrade]()
  protected var quotaIDToTradeIDMap: Map[String, String] = Map[String, String]()

  /*
    Read all trades from Titan and blast our cache
  */
  def updateTradeMap() {
    tradeMap = titanTradesService.getAllTrades()/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.tradeId.toString, t)).toMap
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
      val trade = titanTradesService.getTrade(id)
      tradeMap += trade.tradeId.toString -> trade //.asInstanceOf[EDMPhysicalTrade]
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

trait EnvironmentProvider{
  def getSnapshots() : List[String]
  def environment(snapshotID : String) : Environment
  def updateSnapshotCache() 
  def snapshotNameToID(name : String) : SnapshotID
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] 
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID]
}

class DefaultEnvironmentProvider(marketDataStore : MarketDataStore) extends EnvironmentProvider {
  def getSnapshots() : List[String] = snapshotNameToIDCache.keySet.toList
  def snapshotNameToID(name : String) = snapshotNameToIDCache(name)
  private var snapshotNameToIDCache = Map[String, SnapshotID]()
  private var environmentCache = CacheFactory.getCache("ValuationService.environment", unique = true)
  def environment(snapshotIDName: String): Environment = environmentCache.memoize(
    snapshotIDName,
    {snapshotIDName : String => {
      val snapshotID = snapshotNameToIDCache(snapshotIDName)
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshotID.marketDataSelection, snapshotID.version))
      ClosesEnvironmentRule.createEnv(snapshotID.observationDay, reader).environment
    }}
  )
  private val lock = new Object()

  def updateSnapshotCache() {
    lock.synchronized {
      marketDataStore.snapshots().foreach {
        s: SnapshotID =>
          snapshotNameToIDCache += s.id.toString -> s
      }
    }
  }
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = {
    updateSnapshotCache()
    snapshotNameToIDCache.values.toList.filter(_.observationDay < Day.today()).sortWith(_ > _).headOption.map(_.id.toString)
  }

  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = {
    updateSnapshotCache()
    snapshotNameToIDCache.values.filter {
      starlingSnapshotID =>
        starlingSnapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) && (observationDay.isEmpty || (starlingSnapshotID.observationDay.toJodaLocalDate == observationDay.get))
    }.toList
  }
}

class MockEnvironmentProvider() extends EnvironmentProvider {

  private val snapshotsAndData = Map(
    "Snapshot1" -> (Day(2011, 7, 7), 100.0, 99),
    "Snapshot2" -> (Day(2011, 7, 7), 101.0, 98),
    "Snapshot3" -> (Day(2011, 7, 8), 102.0, 97)
  )
  def getSnapshots() : List[String] = snapshotsAndData.keySet.toList
  
  def environment(snapshotID : String) : Environment = Environment(
    new UnitTestingAtomicEnvironment(
      snapshotsAndData(snapshotID)._1.endOfDay,
      {
        case FixingKey(index, _) => Quantity(snapshotsAndData(snapshotID)._3, index.priceUOM)
        case ForwardPriceKey(market, _, _) => Quantity(snapshotsAndData(snapshotID)._2, market.priceUOM)
        case _: DiscountRateKey => 1.0
      }
    )
  )
  def updateSnapshotCache() {}
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = Some(getSnapshots().head)
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = throw new UnsupportedOperationException
  def snapshotNameToID(name : String) : SnapshotID = throw new UnsupportedOperationException
}


/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider, 
  titanTradeCache : TitanTradeCache, 
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices,
  rabbitEventServices : RabbitEventServices) extends ValuationServiceApi {

  type TradeValuationResult = Either[List[CostsAndIncomeQuotaValuation], String]

  lazy val futuresExchangeByGUID = refData.futuresExchangeByGUID
  lazy val futuresMarketByGUID = refData.futuresMarketByGUID
  val eventHandler = new EventHandler

  rabbitEventServices.eventDemux.addClient(eventHandler)


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
    val env = environmentProvider.environment(snapshotIDString)
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
    val env = environmentProvider.environment(snapshotIDString)
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
    val env = environmentProvider.environment(snapshotIDString)
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
   * value all assignments
   */
  def valueAllAssignments(maybeSnapshotIdentifier : Option[String] = None) : CostAndIncomeAssignmentValuationServiceResults = {
 
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString)
    valueAllAssignments(env, snapshotIDString)
  }

  def valueAllAssignments(env : Environment, snapshotIDString : String) : CostAndIncomeAssignmentValuationServiceResults = {

    val sw = new Stopwatch()

    val inventoryService = logisticsServices.inventoryService.service
    val assignmentService = logisticsServices.assignmentService.service

    val allAssignments = assignmentService.getAllAssignments()
    
    val assignmentsMap : Map[Int, EDMAssignmentItem] = allAssignments.map(a => a.oid.contents -> a).toMap
    val inventory = inventoryService.getAllInventoryLeaves()

    val quotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier).identifier -> q).toMap
    val assignmentIdToQuotaMap : Map[Int, EDMQuota] = allAssignments.map(a => {
      val n = quotaMap(a.quotaName)
      a.oid.contents -> quotaMap(a.quotaName)
    }).toMap

    var tradeValueCache = Map[String, TradeValuationResult]()
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _
    val assignmentValuer = PhysicalMetalAssignmentForward.value(futuresExchangeByGUID, futuresMarketByGUID, assignmentIdToQuotaMap, env, snapshotIDString) _
/*
    val valuations2 =  inventory.flatMap(i => {
     val pAssignment = assignmentsMap(i.purchaseAssignmentId)
     val sAssignment = i.salesAssignmentId match {
        case Some(sAssign) => assignmentsMap(sAssign)
        case None => pAssignment //todo, get proper quota / expected purchase spec
      }
      (pAssignment.oid.contents.toString -> valueAssignment(pAssignment)) :: 
      (sAssignment.oid.contents.toString -> valueAssignment(sAssignment)) :: Nil
    })
*/
    val valuations = inventory.map(i => i.oid.contents.toString -> assignmentValuer(i))

    def valueAssignment(assignment : EDMAssignmentItem) = {
      quotaValue(assignment.quotaName)
    }

    def tradeValue(id: String) : TradeValuationResult = {
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

    log("Valuation took " + sw)
    val (errors, worked) = valuations.partition(_._2 match {
      case Right(_) => true
      case Left(_) => false
    })
    log("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    
    CostAndIncomeAssignmentValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * Return all snapshots for a given observation day, or every snapshot if no day is supplied
   */
  def marketDataSnapshotIDs(observationDay: Option[LocalDate] = None): List[TitanSnapshotIdentifier] = {
    environmentProvider.snapshotIDs(observationDay.map(Day.fromJodaDate)).map {
      starlingSnapshotID => TitanSnapshotIdentifier(starlingSnapshotID.id.toString, starlingSnapshotID.observationDay.toJodaLocalDate)
    }
  }

  private def log(msg: String) = Log.info("ValuationService: " + msg)

  private def resolveSnapshotIdString(maybeSnapshotIdentifier: Option[String] = None) = {
    val snapshotIDString = maybeSnapshotIdentifier.orElse(environmentProvider.mostRecentSnapshotIdentifierBeforeToday()) match {
      case Some(id) => id
      case _ => throw new IllegalStateException("No market data snapshots")
    }
    log("Actual snapshot ID " + snapshotIDString)
    snapshotIDString
  }



  def getTrades(tradeIds : List[String]) : List[EDMPhysicalTrade] = tradeIds.map(titanTradeCache.getTrade)
  def getFuturesExchanges = futuresExchangeByGUID.values
  def getFuturesMarkets = futuresMarketByGUID.values

  /**
   * handler for events
   */
  class EventHandler extends DemultiplexerClient {
    import Event._
    val rabbitPublishChangedValueEvents = publishChangedValueEvents(rabbitEventServices.rabbitEventPublisher) _
    def handle(ev: Event) {
      if (ev == null) Log.warn("Got a null event") else {
        if (Event.TrademgmtSource == ev.source && Event.TradeSubject == ev.subject) { // Must be a trade event from trademgmt
          Log.info("handler: Got a trade event to process %s".format(ev.toString))

          val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
          val tradeIds = tradePayloads.map(p => p.key.identifier)
          Log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))

          ev.verb match {
            case UpdatedEventVerb => {
              val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday() match {
                case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId))
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
          subject = StarlingValuationServiceSubject
          source = StarlingSource
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
 *
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
*/

object ValuationService extends App {

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

  /**
   * Write out EDM trades from trade service (that can be valued successfully) and the ref-data markets and exchanges
   *   so that the file mocked services can use canned data for tests (note this data needs moving into resources to update
   *   canned data for the tests...)
   */
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
