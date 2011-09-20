package starling.services.rpc.valuation

import starling.props.Props
import starling.instrument.PhysicalMetalAssignmentForward
import starling.instrument.PhysicalMetalForward
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
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
import starling.services.rpc.logistics._
import com.trafigura.events.DemultiplexerClient
import com.trafigura.edm.shared.types.TitanId
import java.io.{PrintWriter, FileWriter, BufferedWriter}
import com.trafigura.services.{TitanSerializableDate}
import starling.utils.{Broadcaster, Log, Stopwatch}
import starling.rmi.{RabbitEventDatabase, DefaultRabbitEventDatabase}

//import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.tradestore.TradeStore
import com.trafigura.edm.logistics.inventory.{EDMAssignmentItem, EDMInventoryItem}
import starling.daterange.{Timestamp, Day}
import starling.instrument.Trade


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


case class TitanLogisticsInventoryCache(
    titanLogisticsServices : TitanLogisticsServices,
    titanTradeCache : TitanTradeCache,
    refData : TitanTacticalRefData,
    titanTradeStore : Option[TitanTradeStore]) {

  protected var inventoryMap : Map[String, EDMInventoryItem] = Map[String, EDMInventoryItem]()
  protected var assignmentIDtoInventoryIDMap : Map[String, String] = Map[String, String]()

  private def getAll() = try {
      titanLogisticsServices.inventoryService.service.getAllInventoryLeaves()
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }
  private def getById(id : Int) = try {
    titanLogisticsServices.inventoryService.service.getInventoryById(id).associatedInventory.head
  } catch {
    case e : Throwable => throw new ExternalTitanServiceFailed(e)
  }

  private def tradesForInventory(inventoryID: String): List[Trade] = {
    val tradeConverter = TradeConverter(refData, titanTradeCache)
    getAssignmentsForInventory(inventoryID).map(assignment => tradeConverter.toTrade(assignment))
  }
  private def writeToTradeStore(inventoryID : String) {
    titanTradeStore match {
      case Some(tradeStore) => {
        val trades : Seq[Trade] = tradesForInventory(inventoryID)
        val tradeIDs = trades.map(_.tradeID)
        tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), trades, new Timestamp())
      }
      case None =>
    }
  }
  private def deleteFromTradeStore(inventoryID : String){
    titanTradeStore match {
      case Some(tradeStore) => {
        val tradeIDs = tradesForInventory(inventoryID).map(_.tradeID)
        tradeStore.storeTrades((trade) => tradeIDs.contains(trade.tradeID), Nil, new Timestamp())
      }
      case None =>
    }
  }
  
  def getAssignmentsForInventory(id: String) = {
    val inventory = getInventory(id)
    inventory.purchaseAssignment :: Option(inventory.salesAssignment).toList
  }
  def removeInventory(inventoryID : String) {
    inventoryMap = inventoryMap - inventoryID
    assignmentIDtoInventoryIDMap.filter{ case (_, value) => value != inventoryID}
    deleteFromTradeStore(inventoryID)
  }

  def addInventory(inventoryID : String) {
    inventoryMap += inventoryID -> getInventory(inventoryID)
    addInventoryAssignments(inventoryID)
    writeToTradeStore(inventoryID)
  }

  def addInventoryAssignments(inventoryID : String) {
    val item = inventoryMap(inventoryID)
    val assignmentToInventoryMapItems = (item.purchaseAssignment.oid.contents.toString -> inventoryID) :: Option(item.salesAssignment).toList.map(_.oid.contents.toString -> inventoryID)
    assignmentIDtoInventoryIDMap ++= assignmentToInventoryMapItems
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

  def getInventoryByIds(ids : List[String]) = getAllInventory().filter(i => ids.exists(_ == i.oid.contents.toString))
}


object TitanLogisticsInventoryCache {
  def apply(props : Props,titanTradeCache : TitanTradeCache, refData : TitanTacticalRefData, titanTradeStore : Option[TitanTradeStore]) : TitanLogisticsInventoryCache = {
    new TitanLogisticsInventoryCache(DefaultTitanLogisticsServices(props), titanTradeCache, refData, titanTradeStore)
  }
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
  def environment_(snapshotID : String, marketDay : Option[Day]) : Environment
  def environment(snapshotID : String, marketDay : Option[TitanSerializableDate] = None) : Environment = environment_(snapshotID, marketDay.map{d => Day.fromLocalDate(d.value)})
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
  def environment_(snapshotIDName: String, marketDay : Option[Day]): Environment = environmentCache.memoize(
    snapshotIDName,
    {snapshotIDName : String => {
      val snapshotID = snapshotNameToIDCache(snapshotIDName)
      val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshotID.marketDataSelection, snapshotID.version))
      new ClosesEnvironmentRule().createEnv(marketDay.getOrElse(snapshotID.observationDay), reader).environment
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
  
  def environment_(snapshotID : String, marketDay : Option[Day]) : Environment = Environment(
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

/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider, 
  titanTradeCache : TitanTradeCache, 
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices,
  rabbitEventServices : TitanRabbitEventServices,
  titanInventoryCache : TitanLogisticsInventoryCache,
  titanTradeStore : Option[TradeStore],  // Optional as I don't want to write a mock service for this yet
  rabbitEventDb : RabbitEventDatabase
)
  extends ValuationServiceApi with Log {

  private type TradeValuationResult = Either[String, List[CostsAndIncomeQuotaValuation]]

  private lazy val futuresExchangeByID = refData.futuresExchangeByID
  private lazy val edmMetalByGUID = refData.edmMetalByGUID

  private val eventHandler =
    new TitanEventHandler( rabbitEventServices,
                      this,
                      titanTradeCache,
                      titanInventoryCache,
                      environmentProvider,
                      refData,
                      rabbitEventDb)

  rabbitEventServices.addClient(eventHandler)

  /**
   * Service implementations
   */
  def valueAllTradeQuotas(maybeSnapshotIdentifier : Option[String] = None, observationDate: Option[TitanSerializableDate] = None) : CostAndIncomeQuotaAssignmentValuationServiceResults = {

    log.info("valueAllTradeQuotas called with snapshot id " + maybeSnapshotIdentifier + ", and observation date " + observationDate)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = titanTradeCache.getAllTrades()
    log.info("Got Edm Trade results, trade result count = " + edmTrades.size)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    val tradeValuer = PhysicalMetalForward.valueWithAssignments(futuresExchangeByID, edmMetalByGUID, env, snapshotIDString) _
    log.info("Got %d completed physical trades".format(edmTrades.size))
    sw.reset()
    val valuations = edmTrades.map {
      trade => (trade.titanId.value, tradeValuer(trade))
    }.toMap
    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.values.partition(_ isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    CostAndIncomeQuotaAssignmentValuationServiceResults(snapshotIDString, valuations)
  }


  /**
   * value all edm trade quotas (that are completed) and return a structure containing a
   *   map from tradeId to either a list of CostsAndIncomeQuotaValuation or strings
   */
  def valueAllQuotas(maybeSnapshotIdentifier: Option[String] = None, observationDate: Option[TitanSerializableDate] = None): CostsAndIncomeQuotaOnlyValuationServiceResults = {
    log.info("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = titanTradeCache.getAllTrades()
    log.info("Got Edm Trade results, trade result count = " + edmTrades.size)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByID, edmMetalByGUID, env, snapshotIDString) _
    log.info("Got %d completed physical trades".format(edmTrades.size))
    sw.reset()
    val valuations = edmTrades.map {
      trade => (trade.titanId.value, tradeValuer(trade))
    }.toMap
    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.values.partition(_ isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    CostsAndIncomeQuotaOnlyValuationServiceResults(snapshotIDString, valuations)
  }

  /**
   * value the quotas of a specified trade
   */
  def valueTradeQuotas(tradeId: String, maybeSnapshotIdentifier: Option[String] = None, observationDate: Option[TitanSerializableDate] = None): (String, TradeValuationResult) = {
    log.info("valueTradeQuotas called for trade %d with snapshot id %s".format(tradeId, maybeSnapshotIdentifier))
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)

    val sw = new Stopwatch()

    val edmTradeResult = titanTradeCache.getTrade(TitanId(tradeId))

    log.info("Got Edm Trade result " + edmTradeResult)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
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
  def valueCostables(costableIds: List[String], maybeSnapshotIdentifier: Option[String], observationDate: Option[TitanSerializableDate] = None): CostsAndIncomeQuotaOnlyValuationServiceResults = {
    
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    valueCostables(costableIds, env, snapshotIDString)
  }
  
  def valueCostables(costableIds: List[String], env : Environment, snapshotIDString : String): CostsAndIncomeQuotaOnlyValuationServiceResults = {
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
    
    CostsAndIncomeQuotaOnlyValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * value all assignments by leaf inventory
   */
  def valueAllInventory(maybeSnapshotIdentifier : Option[String] = None, observationDate: Option[TitanSerializableDate] = None) : CostAndIncomeInventoryValuationServiceResults = {
 
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    valueAllAssignments(env, snapshotIDString)
  }
  
  def valueAllAssignments(env : Environment, snapshotIDString : String) : CostAndIncomeInventoryValuationServiceResults = {
    val sw = new Stopwatch()

    val inventory = titanInventoryCache.getAllInventory()

    val quotaNameToQuotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier.value).identifier -> q).toMap

    val assignmentValuer = PhysicalMetalAssignmentForward.value(futuresExchangeByID, edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

    val valuations = inventory.map(i => i.oid.contents.toString -> assignmentValuer(i))
    
    log.info("Valuation took " + sw)
    //val (worked, errors) = valuations.partition(_._2 isRight)
    //log.debug("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    //log.debug("Failed valuation of inventory assignments (%d)...\n%s".format(errors.size, errors.mkString("\n")))

    CostAndIncomeInventoryValuationServiceResults(snapshotIDString, valuations.toMap)
  }

  /**
   * value all inventory assignments by inventory id
   */
  def valueInventory(inventoryIds: List[String], maybeSnapshotIdentifier: Option[String], observationDate: Option[TitanSerializableDate] = None): CostAndIncomeInventoryValuationServiceResults = {

    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    valueInventoryAssignments(inventoryIds, env, snapshotIDString)
  }

  def valueInventoryAssignments(inventoryIds: List[String], env : Environment, snapshotIDString : String) : CostAndIncomeInventoryValuationServiceResults = {
    val sw = new Stopwatch()

    val quotaNameToQuotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier.value).identifier -> q).toMap

    val assignmentValuer = PhysicalMetalAssignmentForward.value(futuresExchangeByID, edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

    val valuations = inventoryIds.map(i => i -> assignmentValuer(titanInventoryCache.getInventory(i)))

    log.info("Valuation took " + sw)
    val (worked, errors) = valuations.partition(_._2 isRight)
    log.info("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)
    log.info("Failed valuation of inventory assignments (%d)...\n%s".format(errors.size, errors.mkString("\n")))

    CostAndIncomeInventoryValuationServiceResults(snapshotIDString, valuations.toMap)
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

  lazy val vs = StarlingInit.runningDevInstance.valuationService

  val quotaValuations = vs.valueAllTradeQuotas()
  val (worked, failed) = quotaValuations.valuationResults.values.partition({ case Right(_) => true; case Left(_) => false })
  println("Worked \n" + worked.mkString("\n") + "\nFailed \n" + failed.mkString("\n"))

  System.exit(0)
  val valuations = vs.valueAllQuotas()
  //val (worked, _) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })

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

  StarlingInit.runningDevInstance.stop

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