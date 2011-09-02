package starling.services.rpc.valuation

import starling.props.Props
import starling.instrument.PhysicalMetalAssignmentForward
import starling.instrument.PhysicalMetalForward
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import starling.utils.{Log, Stopwatch}
import starling.services.StarlingInit
import com.trafigura.services.valuation._
import starling.services.rpc.refdata._
import com.trafigura.tradinghub.support.ModelObject
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import java.lang.Exception
import com.trafigura.shared.events._
import org.joda.time.LocalDate
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
import starling.curves.IndexFixingKey
import starling.titan._
import com.trafigura.edm.logistics.inventory.EDMInventoryItem
import starling.services.rpc.logistics._
import com.trafigura.events.DemultiplexerClient
import com.trafigura.edm.shared.types.TitanId
import com.trafigura.services.BouncyRMIServiceApi._
import com.trafigura.services.BouncyRMIServiceApi

/**
 * Trade cache provide trade map lookup by trade id and also a quota id to trade map lookup
 */
case class DefaultTitanTradeCache(props : Props) extends TitanTradeCache with Log {
  protected var tradeMap = Map[TitanId, EDMPhysicalTrade]()
  protected var quotaIDToTradeIDMap = Map[String, TitanId]()

  private lazy val titanTradesService = new DefaultTitanServices(props).titanGetEdmTradesService
  private def getAll() = try {
      titanTradesService.getAll()
  } catch {
    case e : Throwable => { log.error("Error getting Titan EDM trades ", e); throw new ExternalTitanServiceFailed(e) }
  }

  private def getById(id : TitanId) = try {
      titanTradesService.get(id)
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }

  /*
    Read all trades from Titan and blast our cache
  */
  def updateTradeMap() {
    val sw = new Stopwatch()
    val edmTradeResult = getAll()
    log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)

    val validTrades = edmTradeResult.results.filter(tr => tr.trade != null).map(_.trade.asInstanceOf[EDMPhysicalTrade])

    // temporary code, trademgmt are sending us null titan ids
    val (nullIds, validIds) = validTrades.span(_.titanId == null)
    if (nullIds.size > 0) {
      log.error("Null Titan trade IDs found!")
      log.error("null ids \n%s\n%s".format(nullIds, validIds))
      //assert(false, "Null titan ids found - fatal error")
    }
    tradeMap = validTrades/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.titanId, t)).toMap
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

  def getTrade(id: TitanId): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = getById(id)
      tradeMap += trade.titanId -> trade.asInstanceOf[EDMPhysicalTrade]
      addTradeQuotas(id)
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): TitanId = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}

trait TitanLogisticsInventoryCache {
  protected var inventoryMap: Map[String, EDMInventoryItem]
  protected var assignmentIDtoInventoryIDMap : Map[String, String]
  def getInventory(id: String): EDMInventoryItem
  def getAllInventory(): List[EDMInventoryItem]
  def removeInventory(id : String) {
    inventoryMap = inventoryMap - id
    assignmentIDtoInventoryIDMap.filter{ case (_, value) => value != id}
  }

  def addInventory(id : String) {
    inventoryMap += id -> getInventory(id)
    addInventoryAssignments(id)
  }

  def addInventoryAssignments(id : String) {
    val item = inventoryMap(id)
    val assignmentToInventoryMapItems = (item.purchaseAssignment.oid.contents.toString, id) :: { if (item.salesAssignment != null) (item.salesAssignment.oid.contents.toString -> id) :: Nil else Nil }
    assignmentIDtoInventoryIDMap ++= assignmentToInventoryMapItems
  }

  def inventoryIDFromAssignmentID(id: String): String
}


case class DefaultTitanLogisticsInventoryCache(props : Props) extends TitanLogisticsInventoryCache {
  protected var inventoryMap : Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]()

  private val titanTradeService = new DefaultTitanServices(props)
  private val titanLogisticsServices = DefaultTitanLogisticsServices(props)
  private def getAll() = try {
      titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }
  private def getById(id : Int) = try {
    titanLogisticsServices.inventoryService.service.getInventoryById(id)
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }

  // Read all inventory from Titan and blast our cache
  def updateMap() {
    val sw = new Stopwatch()
    val edmInventoryResult = getAll()
    inventoryMap = edmInventoryResult.map(i => (i.oid.contents.toString, i)).toMap
    inventoryMap.keySet.foreach(addInventoryAssignments)
  }

  def getAllInventory() : List[EDMInventoryItem] = {
    if (inventoryMap.size > 0) {
      inventoryMap.values.toList
    }
    else {
      updateMap()
      inventoryMap.values.toList
    }
  }

  def getInventory(id: String) : EDMInventoryItem = {
    if (inventoryMap.contains(id)) {
      inventoryMap(id)
    }
    else {
      val item = getById(id.toInt)
      inventoryMap += item.oid.contents.toString -> item
      addInventoryAssignments(id)
      inventoryMap(id)
    }
  }

  def inventoryIDFromAssignmentID(id: String): String = {
    if (!assignmentIDtoInventoryIDMap.contains(id))
      updateMap()
    assignmentIDtoInventoryIDMap.get(id) match {
      case Some(id) => id
      case None => throw new Exception("Missing inventory " + id)
    }
  }
}

case class TitanLogisticsServiceBasedInventoryCache(titanLogisticsServices : TitanLogisticsServices) extends TitanLogisticsInventoryCache {
  protected var inventoryMap: Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]()

  // Read from Titan and blast our cache
  def updateMap() {
    val sw = new Stopwatch()
    val edmInventoryResult = titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
    inventoryMap = edmInventoryResult.map(i => (i.oid.contents.toString, i)).toMap
  }

  def getAllInventory() : List[EDMInventoryItem] = {
    if (inventoryMap.size > 0) {
      inventoryMap.values.toList
    }
    else {
      updateMap()
      inventoryMap.values.toList
    }
  }

  def getInventory(id: String) : EDMInventoryItem = {
    if (inventoryMap.contains(id)) {
      inventoryMap(id)
    }
    else {
      val item = titanLogisticsServices.inventoryService.service.getInventoryById(id.toInt)
      inventoryMap += item.oid.contents.toString -> item
      inventoryMap(id)
    }
  }

  def inventoryIDFromAssignmentID(id: String): String = {
    if (!assignmentIDtoInventoryIDMap.contains(id))
      updateMap()
    assignmentIDtoInventoryIDMap.get(id) match {
      case Some(id) => id
      case None => throw new Exception("Missing inventory " + id)
    }
  }

  def getInventoryByIds(ids : List[String]) = getAllInventory().filter(i => ids.exists(_ == i.oid.contents.toString))
}

trait TitanTradeService {
  def getTrade(id : TitanId) : EDMPhysicalTrade
  def getAllTrades() : List[EDMPhysicalTrade]
}

class DefaultTitanTradeService(titanServices : TitanServices) extends TitanTradeService with Log {

  def getTrade(id : TitanId) : EDMPhysicalTrade = {
    titanServices.titanGetEdmTradesService.get(id).asInstanceOf[EDMPhysicalTrade]
  }

  def getAllTrades() : List[EDMPhysicalTrade] = {
    val sw = new Stopwatch()
    val edmTradeResult = titanServices.titanGetEdmTradesService.getAll()
    log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade])
  }
}

/**
 * Trade cache using supplied ref data
 */
case class TitanTradeServiceBasedTradeCache(titanTradesService : TitanTradeService) extends TitanTradeCache {

  protected var tradeMap = Map[TitanId, EDMPhysicalTrade]()
  protected var quotaIDToTradeIDMap = Map[String, TitanId]()

  // Read all trades from Titan and blast our cache
  def updateTradeMap() {
    tradeMap = titanTradesService.getAllTrades()/*.filter(pt => pt.tstate == CompletedTradeTstate)*/.map(t => (t.titanId, t)).toMap
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

  def getTrade(id: TitanId): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = titanTradesService.getTrade(id)
      tradeMap += trade.titanId -> trade
      tradeMap(id)
    }
  }

  def tradeIDFromQuotaID(quotaID: String): TitanId = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }
}

trait EnvironmentProvider {
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
    snapshotNameToIDCache.values.toList.filter(_.observationDay < Day.today).sortWith(_ > _).headOption.map(_.id.toString)
  }

  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = {
    updateSnapshotCache()
    snapshotNameToIDCache.values.filter {
      starlingSnapshotID =>
        starlingSnapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals) && (observationDay.isEmpty || (starlingSnapshotID.observationDay == observationDay.get))
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
        case IndexFixingKey(index, _) => Quantity(snapshotsAndData(snapshotID)._3, index.priceUOM)
        case ForwardPriceKey(market, _, _) => Quantity(snapshotsAndData(snapshotID)._2, market.priceUOM)
        case _: DiscountRateKey => new Quantity(1.0)
      }
    )
  )
  def updateSnapshotCache() {}
  def mostRecentSnapshotIdentifierBeforeToday(): Option[String] = Some(getSnapshots().head)
  def snapshotIDs(observationDay : Option[Day]) : List[SnapshotID] = throw new UnsupportedOperationException
  def snapshotNameToID(name : String) : SnapshotID = throw new UnsupportedOperationException
}

//class ValuationServiceStub extends ValuationServicer()

/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider, 
  titanTradeCache : TitanTradeCache, 
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices,
  rabbitEventServices : TitanRabbitEventServices,
  titanInventoryCache : TitanLogisticsInventoryCache) extends ValuationServiceApi with Log {

  type TradeValuationResult = Either[String, List[CostsAndIncomeQuotaValuation]]

  lazy val futuresExchangeByID = refData.futuresExchangeByID
  lazy val edmMetalByGUID = refData.edmMetalByGUID
  lazy val uomById = refData.uomById
  implicit lazy val uomIdToName : Map[Int, String] = uomById.values.map(e => e.oid -> e.name).toMap

  val eventHandler = new EventHandler

  rabbitEventServices.addClient(eventHandler)


  /**
   * value all edm trade quotas (that are completed) and return a structure containing a
   *   map from tradeId to either a list of CostsAndIncomeQuotaValuation or strings
   */
  def valueAllQuotas(maybeSnapshotIdentifier: Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    log.info("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = titanTradeCache.getAllTrades()
    log.info("Got Edm Trade results, trade result count = " + edmTrades.size)
    val env = environmentProvider.environment(snapshotIDString)
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByID, edmMetalByGUID, env, snapshotIDString) _
    log.info("Got %d completed physical trades".format(edmTrades.size))
    sw.reset()
    val valuations = edmTrades.map {
      trade => (trade.titanId.value, tradeValuer(trade))
    }.toMap
    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.values.partition(_ isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, valuations)
  }

  /**
   * value the quotas of a specified trade
   */
  def valueTradeQuotas(tradeId: String, maybeSnapshotIdentifier: Option[String] = None): (String, TradeValuationResult) = {
    log.info("valueTradeQuotas called for trade %d with snapshot id %s".format(tradeId, maybeSnapshotIdentifier))
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)

    val sw = new Stopwatch()

    val edmTradeResult = titanTradeCache.getTrade(TitanId(tradeId))

    log.info("Got Edm Trade result " + edmTradeResult)
    val env = environmentProvider.environment(snapshotIDString)
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByID, edmMetalByGUID, env, snapshotIDString) _

    val edmTrade: EDMPhysicalTrade = edmTradeResult.asInstanceOf[EDMPhysicalTrade]
    log.info("Got %s physical trade".format(edmTrade.toString))
    sw.reset()
    val valuation = tradeValuer(edmTrade)
    log.info("Valuation took " + sw)

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

    // value all trades (and therefor all quotas) keyed by trade ids
    val idsToUse = costableIds match {
      case Nil | null => titanTradeCache.getAllTrades().map{trade => trade.titanId.value}
      case list => list
    }

    var tradeValueCache = Map[String, TradeValuationResult]()
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByID, edmMetalByGUID, env, snapshotIDString) _

    def tradeValue(id : String): TradeValuationResult = {
      if (!tradeValueCache.contains(id))
        tradeValueCache += (id -> tradeValuer(titanTradeCache.getTrade(TitanId(id))))
      tradeValueCache(id)
    }

    def quotaValue(id: String) = {
      tradeValue(titanTradeCache.tradeIDFromQuotaID(id).value) match {
        case Right(list) => Right(list.filter(_ .quotaID == id))
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

    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.partition(_._2 isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    
    CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * value all assignments by leaf inventory
   */
  def valueAllAssignments(maybeSnapshotIdentifier : Option[String] = None) : CostAndIncomeAssignmentValuationServiceResults = {
 
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString)
    valueAllAssignments(env, snapshotIDString)
  }
  
  def valueAllAssignments(env : Environment, snapshotIDString : String) : CostAndIncomeAssignmentValuationServiceResults = {
    val sw = new Stopwatch()

    val inventory = titanInventoryCache.getAllInventory()

    val quotaNameToQuotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier.value).identifier -> q).toMap

    val assignmentValuer = PhysicalMetalAssignmentForward.value(futuresExchangeByID, edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

    val valuations = inventory.map(i => i.oid.contents.toString -> assignmentValuer(i))
    
    log.info("Valuation took " + sw)
    //val (worked, errors) = valuations.partition(_._2 isRight)
    //log.debug("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    //log.debug("Failed valuation of inventory assignments (%d)...\n%s".format(errors.size, errors.mkString("\n")))

    CostAndIncomeAssignmentValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * value all inventory assignments by inventory id
   */
  def valueInventoryAssignments(inventoryIds: List[String], maybeSnapshotIdentifier: Option[String]): CostAndIncomeAssignmentValuationServiceResults = {

    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString)
    valueInventoryAssignments(inventoryIds, env, snapshotIDString)
  }

  def valueInventoryAssignments(inventoryIds: List[String], env : Environment, snapshotIDString : String) : CostAndIncomeAssignmentValuationServiceResults = {
    val sw = new Stopwatch()

    val quotaNameToQuotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier.value).identifier -> q).toMap

    val assignmentValuer = PhysicalMetalAssignmentForward.value(futuresExchangeByID, edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

    val valuations = inventoryIds.map(i => i -> assignmentValuer(titanInventoryCache.getInventory(i)))

    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.partition(_._2 isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    log.info("Failed valuation of inventory assignments (%d)...\n%s".format(errors.size, errors.mkString("\n")))

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

  private def resolveSnapshotIdString(maybeSnapshotIdentifier: Option[String] = None) = {
    val snapshotIDString = maybeSnapshotIdentifier.orElse(environmentProvider.mostRecentSnapshotIdentifierBeforeToday()) match {
      case Some(id) => id
      case _ => throw new IllegalStateException("No market data snapshots")
    }
    log.info("Actual snapshot ID " + snapshotIDString)
    snapshotIDString
  }

  // accessors for ref-data mappings
  def getTrades(tradeIds : List[String]) : List[EDMPhysicalTrade] = tradeIds.map(id => TitanId(id)).map(titanTradeCache.getTrade)
  def getFuturesExchanges = futuresExchangeByID.values
  def getMetals = edmMetalByGUID.values
  def getUoms = uomById.values
    

  /**
   * handler for Titan rabbit events
   */
  class EventHandler extends DemultiplexerClient {
    import Event._
    lazy val rabbitPublishChangedValueEvents = publishStarlingChangedValueEvents(rabbitEventServices.rabbitEventPublisher)
    lazy val rabbitPublishNewValuationEvents = publishCreatedValuationEvents(rabbitEventServices.rabbitEventPublisher) _
    def handle(ev: Event) {
      if (ev == null) log.warn("Got a null event") else {
        if (TrademgmtSource == ev.source && (TradeSubject == ev.subject || NeptuneTradeSubject == ev.subject)) { // Must be some form of trade event from trademgmt source
          tradeMgmtTradeEventHander(ev)
        }
        else if (LogisticsSource == ev.source && (EDMLogisticsSalesAssignmentSubject == ev.subject || EDMLogisticsInventorySubject == ev.subject)) {
          logisticsAssignmentEventHander(ev)
        }
      }
    }

    /**
     * handler for trademgnt trade events
     */
    def tradeMgmtTradeEventHander(ev: Event) = {
      log.info("handler: Got a trade event to process %s".format(ev.toString))

      val tradePayloads = ev.content.body.payloads.filter(p => Event.RefinedMetalTradeIdPayload == p.payloadType)
      val tradeIds = tradePayloads.map(p => p.key.identifier)
      val titanIds = tradeIds.map(id =>TitanId(id))
      log.info("Trade event received for ids { %s }".format(tradeIds.mkString(", ")))

      ev.subject match {
        case TradeSubject => {
          ev.verb match {
            case UpdatedEventVerb => {
              val (snapshotIDString, env) = getSnapshotAndEnv
              val originalTradeValuations = valueCostables(tradeIds, env, snapshotIDString)
              titanIds.foreach{ id => titanTradeCache.removeTrade(id); titanTradeCache.addTrade(id)}
              val newTradeValuations = valueCostables(tradeIds, env, snapshotIDString)
              val changedIDs = tradeIds.filter{id => newTradeValuations.tradeResults(id) != originalTradeValuations.tradeResults(id)}

              if (changedIDs != Nil)
                rabbitPublishChangedValueEvents(changedIDs, RefinedMetalTradeIdPayload)

              log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
            }
            case CreatedEventVerb => {
              Log.info("New event received for %s".format(tradeIds))
              titanIds.foreach(titanTradeCache.addTrade)
            }
            case CancelledEventVerb | RemovedEventVerb => {
              Log.info("Cancelled / deleted event received for %s".format(titanIds))
              titanIds.foreach(titanTradeCache.removeTrade)
            }
          }
        }
        // handle publishing of valuation status events (events saying if a new (completed) trade can be valued successfully or not)
        case NeptuneTradeSubject => {
          val completed = ev.content.body.payloads.filter(p => TradeStatusPayload == p.payloadType).filter(p => p.key.identifier == "completed").size > 0
          if (completed) {
            val (snapshotIDString, env) = getSnapshotAndEnv
            val tradeValuations = valueCostables(tradeIds, env, snapshotIDString)
            val valuationResults = tradeValuations.tradeResults.map(vr => {
              vr._2 match {
                case Right(v) => (vr._1, true)
                case Left(e) => (vr._1, false)
              }
            }).toList
            rabbitPublishNewValuationEvents(valuationResults)
          }
        }
      }
    }

    /**
     * handler for logistics assignment events
     */
    def logisticsAssignmentEventHander(ev: Event) = {
      log.info("handler: Got an assignment event to process %s".format(ev.toString))

      val payloads = ev.content.body.payloads
      val ids : List[String] = if (Event.EDMLogisticsSalesAssignmentSubject == ev.subject) {
        payloads.map(p => titanInventoryCache.inventoryIDFromAssignmentID(p.key.identifier)) // map back to inventory id
      }
      else if (Event.EDMLogisticsInventorySubject == ev.subject) {
        payloads.map(p => p.key.identifier)
      }
      else Nil

      log.info("Assignment event received for ids { %s }".format(ids.mkString(", ")))

      ev.verb match {
        case UpdatedEventVerb => {
          val (snapshotIDString, env) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday() match {
            case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId))
            case None => ("No Snapshot found",  Environment(NullAtomicEnvironment((Day.today - 1).startOfDay)))
          }

          val originalInventoryAssignmentValuations = valueInventoryAssignments(ids, env, snapshotIDString)
          ids.foreach{ id => titanInventoryCache.removeInventory(id); titanInventoryCache.addInventory(id)}
          val newInventoryAssignmentValuations = valueInventoryAssignments(ids, env, snapshotIDString)
          val changedIDs = ids.filter {id => newInventoryAssignmentValuations.assignmentValuationResults(id) != originalInventoryAssignmentValuations.assignmentValuationResults(id) }

          if (changedIDs != Nil) {
            rabbitPublishChangedValueEvents(changedIDs, EDMLogisticsInventoryIdPayload)
          }

          log.info("Assignments revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
        }
        case CreatedEventVerb => {
          Log.info("New event received for %s".format(ids))
          if (Event.EDMLogisticsInventorySubject == ev.subject) {
            ids.foreach(titanInventoryCache.addInventory)
          }
        }
        case CancelledEventVerb | RemovedEventVerb => {
          Log.info("Cancelled / deleted event received for %s".format(ids))
          if (Event.EDMLogisticsInventorySubject == ev.subject) {
            ids.foreach(titanInventoryCache.removeInventory)
          }
        }
      }
    }

    /**
     * handler for snapshot events
     */
    def marketDataSnapshotEventHander(ev: Event) = {
      log.info("handler: Got a snapshot event to process %s".format(ev.toString))

      /**
       * logic:
       *   take the most recent previous snapshot of the day and compare values using that snapshot and the new one, if different send events containing ids
       *   else if there is no previous snapshot for today then do nothing
       */
      val payloads = ev.content.body.payloads.filter(p => StarlingSnapshotIdPayload == p.payloadType)
      val ids = payloads.map(p => p.key.identifier)
      val newSnapshotId = payloads.map(p => p.key.identifier).max
      val todaysSnapshots : List[SnapshotID] = environmentProvider.snapshotIDs(Some(Day.today)).sortWith(_.id > _.id)
      log.info("Snapshot event received for ids { %s }, using '%s'".format(ids.mkString(", "), newSnapshotId))

      if (todaysSnapshots.size > 1) {
        ev.subject match {
          case StarlingMarketDataSnapshotIDSubject => {
            ev.verb match {
              case CreatedEventVerb => {
                val previousSnapshotId = todaysSnapshots(1).id
                log.info("New marketData snapshot event, revaluing received event using old snapshot %s new snapshot %s".format(previousSnapshotId, newSnapshotId))
                val previousEnv = environmentProvider.environment(previousSnapshotId)
                val newEnv = environmentProvider.environment(newSnapshotId)

                val originalTradeValuations = valueCostables(Nil, previousEnv, previousSnapshotId)
                val newTradeValuations = valueCostables(Nil, newEnv, newSnapshotId)
                val tradeIds = newTradeValuations.tradeResults.keys.toList
                val changedTradeIDs = tradeIds.filter(id => newTradeValuations.tradeResults(id) != originalTradeValuations.tradeResults(id))

                log.info("Trades revalued for new snapshot %s, number of changed valuations %d".format(newSnapshotId, changedTradeIDs.size))

                if (changedTradeIDs != Nil)
                  rabbitPublishChangedValueEvents(changedTradeIDs, RefinedMetalTradeIdPayload)

                val originalInventoryValuations = valueInventoryAssignments(Nil, previousEnv, previousSnapshotId)
                val newInventoryValuations = valueInventoryAssignments(Nil, newEnv, newSnapshotId)
                val inventoryIds = newInventoryValuations.assignmentValuationResults.keys.toList
                val changedInventoryValueIDs = inventoryIds.filter(id => newInventoryValuations.assignmentValuationResults(id) != newInventoryValuations.assignmentValuationResults(id))

                log.info("Inventory assignments revalued for new snapshot %s, number of changed valuations %d".format(newSnapshotId, changedInventoryValueIDs.size))

                if (changedInventoryValueIDs != Nil)
                  rabbitPublishChangedValueEvents(changedInventoryValueIDs, RefinedMetalTradeIdPayload)
              }
              case UpdatedEventVerb => {
                log.info("Unhandled event for updated snapshot %s".format(ids))
              }
              case CancelledEventVerb | RemovedEventVerb => {
                log.info("Unhandled cancelled / deleted event received for %s".format(ids))
              }
            }
          }
        }
      }
    }

    def getSnapshotAndEnv : (String, Environment) = environmentProvider.mostRecentSnapshotIdentifierBeforeToday() match {
      case Some(snapshotId) => (snapshotId, environmentProvider.environment(snapshotId))
      case None => ("No Snapshot found",  Environment(NullAtomicEnvironment((Day.today - 1).startOfDay)))
    }

    // publish the valuation updated event contaning payloads of the trade id's whose trade valuations have changed
    private val publishStarlingChangedValueEvents = publishChangedValueEvents(StarlingSource, StarlingValuationServiceSubject) _
    private def publishChangedValueEvents(source : String, subject : String)
                                         (eventPublisher : Publisher)(ids : List[String], payloadTypeParam : String = RefinedMetalTradeIdPayload) : Unit  = {

      val payloadDetails = ids.map(id => (payloadTypeParam, id))
      val payloads = createPayloads(source)(payloadDetails)
      val events = createEvents(source, subject)(UpdatedEventVerb, payloads)
      eventPublisher.publish(events)
    }

    private def publishCreatedValuationEvents(eventPublisher : Publisher)(newValuations : List[(String, Boolean)]) = {
      val newValuationPayloads : List[(String, String)] = newValuations.flatMap(e => (RefinedMetalTradeIdPayload, e._1) :: (StarlingNewValuationServiceStatusPayload, e._2.toString) :: Nil)
      val payloads = createStarlingPayloads(newValuationPayloads)
      val newValuationEvents = createValuationServiceEvents(CreatedEventVerb, payloads)
      eventPublisher.publish(newValuationEvents)
    }

    private val createValuationServiceEvents = createEvents(StarlingSource, StarlingValuationServiceSubject) _
    private def createEvents(source : String, subject : String)(verb : EventVerbEnum, payloads : List[Payload]) : JSONArray = {
      val keyIdentifier = System.currentTimeMillis.toString
      val ev = EventFactory().createEvent(subject, verb, source, keyIdentifier, payloads)
      ||> { new JSONArray } { r => r.put(ev.toJson) }
    }

    private val createStarlingPayloads = createPayloads(StarlingSource) _
    private def createPayloads(source : String)(payloads : List[(String, String)]) : List[Payload] = {
      payloads.map(p => EventPayloadFactory().createPayload(p._1, source, p._2))
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

  lazy val vs = StarlingInit.devInstance.valuationService

  val valuations = vs.valueAllQuotas()

  val (worked, _) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })

  val valuedTradeIds = valuations.tradeResults.collect{ case (id, Right(v)) => id }.toList
  val valuedTrades = vs.getTrades(valuedTradeIds)
  val markets = vs.getMetals.toList
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
}


/**
 * Run up a test instance of the server and invoke the valuation service operations to test services using mock data
 *   for service dependencies
 */
object ValuationServiceCompTest extends App {

  println("Running main for valuation service tests")
  val sw = Stopwatch()

  val server = StarlingInit.testInstance
  lazy val vs = server.valuationService

  println("Took %s to start the test server".format(sw))

  BouncyRMIServiceApi().using { valuationServiceRMI : ValuationServiceApi =>
    val runCount = 5

    def run[T](desc : String, f : () => T) = {
      (1 to runCount).map {n =>
        val sw = Stopwatch()
        val valuations = f()
        println("direct call for %s, run %d took %s".format(desc, n, sw))
        (n, sw.toString, valuations)
      }
    }

    def showResults[T <: Either[_, _]](ls : List[T], desc : String) = {
      val (worked, errors) = ls.partition(_.isRight)

      println("\nCalled valueAll for %s, %d worked, %d failed, took %s".format(desc, worked.size, errors.size, sw))

      //println("\nSuccessful %s valuations:\n".format(desc))
      //worked.foreach(println)

      //println("\nFailed %s valuations:\n".format(desc))
      //errors.foreach(println)
    }

    val directQuotaResults = run("Quota", () => vs.valueAllQuotas())
    val rmiQuotaResults = run("Quota", () => valuationServiceRMI.valueAllQuotas())

    val directInventoryResults = run("Inventory", () => vs.valueAllAssignments())
    val rmiInventoryResults = run("Inventory", () => valuationServiceRMI.valueAllAssignments())

//    val quotaValuations = valuationServiceRMI.valueAllQuotas()
//    showResults(quotaValuations.tradeResults.values.toList, "Quotas")

//    val inventoryValuations = vs.valueAllAssignments()
//    showResults(inventoryValuations.assignmentValuationResults.values.toList, "Inventory")

    println("\nDirect quota results")
    directQuotaResults.map(r => (r._1, r._2, r._3.tradeResults.values.size, r._3.tradeResults.values.partition(_.isRight)._1.size)).foreach(println)

    println("\nRMI quota results")
    rmiQuotaResults.map(r => (r._1, r._2, r._3.tradeResults.values.size, r._3.tradeResults.values.partition(_.isRight)._1.size)).foreach(println)

    println("\nDirect inventory results")
    directInventoryResults.map(r => (r._1, r._2, r._3.assignmentValuationResults.values.size, r._3.assignmentValuationResults.values.partition(_.isRight)._1.size)).foreach(println)

    println("\nRMI inventory results")
    rmiInventoryResults.map(r => (r._1, r._2, r._3.assignmentValuationResults.values.size, r._3.assignmentValuationResults.values.partition(_.isRight)._1.size)).foreach(println)
  }

  server.stop
}
