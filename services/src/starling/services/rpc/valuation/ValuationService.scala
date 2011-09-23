package starling.services.rpc.valuation

import starling.instrument.PhysicalMetalAssignmentForward
import starling.instrument.PhysicalMetalForward
import starling.curves.Environment
import starling.services.StarlingInit
import com.trafigura.services.valuation._
import com.trafigura.tradinghub.support.ModelObject
import com.trafigura.edm.trades.{PhysicalTrade => EDMPhysicalTrade}
import java.lang.Exception
import org.joda.time.LocalDate
import starling.services.rabbit._
import com.trafigura.tradecapture.internal.refinedmetal.Market
import com.trafigura.tradecapture.internal.refinedmetal.Metal
import starling.titan._
import com.trafigura.edm.shared.types.TitanId
import java.io.{FileWriter, BufferedWriter}
import com.trafigura.services.{TitanSerializableDate}
import starling.utils.{Log, Stopwatch}
import starling.rmi.RabbitEventDatabase

import starling.tradestore.TradeStore
import starling.daterange.Day




/**
 * Valuation service implementations
 */
class ValuationService(
  environmentProvider : EnvironmentProvider, 
  titanTradeCache : TitanTradeCache, 
  refData : TitanTacticalRefData,
  logisticsServices : TitanLogisticsServices,
  rabbitEventServices : TitanRabbitEventServices,
  titanInventoryCache : DefaultTitanLogisticsInventoryCache,
  titanTradeStore : Option[TradeStore],  // Optional as I don't want to write a mock service for this yet
  rabbitEventDb : RabbitEventDatabase
)
  extends ValuationServiceApi with Log {


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
    val tradeValuer = PhysicalMetalForward.valueWithAssignments(refData.futuresExchangeByID, refData.edmMetalByGUID, env, snapshotIDString) _
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
   * Service implementations
   */
  def valueSingleTradeQuotas(tradeID : String, maybeSnapshotIdentifier : Option[String] = None, observationDate: Option[TitanSerializableDate] = None) : (String, Either[String, List[QuotaValuation]]) = {

    log.info("valueAllTradeQuotas called with snapshot id " + maybeSnapshotIdentifier + ", and observation date " + observationDate)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environmentProvider.environment(snapshotIDString, observationDate)
    valueSingleTradeQuotas(tradeID, env, snapshotIDString)
  }

  def valueSingleTradeQuotas(tradeId : String, env : Environment, snapshotIDString : String): (String, Either[String, List[QuotaValuation]]) = {
      val tradeValuer = PhysicalMetalForward.valueWithAssignments(refData.futuresExchangeByID, refData.edmMetalByGUID, env, snapshotIDString) _
      val edmTradeResult = titanTradeCache.getTrade(TitanId(tradeId))
      log.debug("Got Edm Trade result " + edmTradeResult)
      val edmTrade: EDMPhysicalTrade = edmTradeResult.asInstanceOf[EDMPhysicalTrade]
      log.debug("Got %s physical trade".format(edmTrade.toString))
      (snapshotIDString, tradeValuer(edmTrade))
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

    val inventory = titanInventoryCache.getAll()

    val quotaNameToQuotaMap = titanTradeCache.getAllTrades().flatMap(_.quotas).map(q => NeptuneId(q.detail.identifier.value).identifier -> q).toMap

    val assignmentValuer = PhysicalMetalAssignmentForward.value(refData.futuresExchangeByID, refData.edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

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

    val assignmentValuer = PhysicalMetalAssignmentForward.value(refData.futuresExchangeByID, refData.edmMetalByGUID, quotaNameToQuotaMap, env, snapshotIDString) _

    val valuations = inventoryIds.map(i => i -> assignmentValuer(titanInventoryCache.getByID(i)))

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
  def getFuturesExchanges = refData.futuresExchangeByID.values
  def getMetals = refData.edmMetalByGUID.values
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


//object ValuationService extends App {
//
//  import org.codehaus.jettison.json.JSONObject
//
//  lazy val vs = StarlingInit.runningDevInstance.valuationService
//
//  val quotaValuations = vs.valueAllTradeQuotas()
//  val (worked, failed) = quotaValuations.valuationResults.values.partition({ case Right(_) => true; case Left(_) => false })
//  println("Worked \n" + worked.mkString("\n") + "\nFailed \n" + failed.mkString("\n"))
//
//  System.exit(0)
//  val valuations = vs.valueAllTradeQuotas()
//  //val (worked, _) = valuations.tradeResults.values.partition({ case Right(_) => true; case Left(_) => false })
//
//  val valuedTradeIds = valuations.valuationResults.collect{ case (id, Right(v)) => id }.toList
//  val valuedTrades = vs.getTrades(valuedTradeIds)
//  val markets = vs.getMetals.toList
//  val exchanges = vs.getFuturesExchanges.toList
//
//  /**
//   * Write out EDM trades from trade service (that can be valued successfully) and the ref-data markets and exchanges
//   *   so that the file mocked services can use canned data for tests (note this data needs moving into resources to update
//   *   canned data for the tests...)
//   */
//  val tradesFile = "/tmp/edmTrades.json"
//  val marketsFile = "/tmp/markets.json"
//  val exchangesFile = "/tmp/exchanges.json"
//
//  writeJson(tradesFile, valuedTrades)
//  writeJson(marketsFile, markets)
//  writeJson(exchangesFile, exchanges)
//
//  val loadedMarkets = loadJsonValuesFromFile(marketsFile).map(s => Metal.fromJson(new JSONObject(s)).asInstanceOf[Metal])
//  val loadedExchanges = loadJsonValuesFromFile(exchangesFile).map(s => Market.fromJson(new JSONObject(s)).asInstanceOf[Market])
//  val loadedTrades = loadJsonValuesFromFile(tradesFile).map(s => EDMPhysicalTrade.fromJson(new JSONObject(s)).asInstanceOf[EDMPhysicalTrade])
//
//  StarlingInit.runningDevInstance.stop
//
//  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T]) {
//    try {
//      val fStream = new FileWriter(fileName)
//      val bWriter = new BufferedWriter(fStream)
//      objects.foreach(obj => bWriter.write(obj.toJson().toString() + "\n" ))
//      bWriter.flush()
//      fStream.close()
//    }
//    catch {
//      case ex : Exception => println("Error: " + ex.getMessage())
//    }
//  }
//
//  import scala.io.Source._
//  def loadJsonValuesFromFile(fileName : String) : List[String] =
//    fromFile(fileName).getLines.toList
//}
