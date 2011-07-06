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
import starling.services.rpc.refdata.TacticalRefData
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

trait TitanTradeCache{
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

  val titanTradesService = new TacticalRefData(props).titanGetEdmTradesService
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
//class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends TacticalRefData(props: Props) with ValuationServiceApi {
class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends ValuationServiceApi {

  type TradeValuationResult = Either[List[CostsAndIncomeQuotaValuation], String]
  

  val tradeCache = new DefaultTitanTradeCache(props)
  val futuresExchangeByGUID = new TacticalRefData(props).futuresExchangeByGUID
  val futuresMarketByGUID = new TacticalRefData(props).futuresMarketByGUID
  val eventHandler = new EventHandler

  RabbitEvents.eventDemux.addClient(eventHandler)



  /**
   * value all edm trade quotas (that are completed) and return a structure containing a
   *   map from tradeId to either a list of CostsAndIncomeQuotaValuation or strings
   */
  def valueAllQuotas(maybeSnapshotIdentifier: Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    log("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = tradeCache.getAllTrades()
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

    val edmTradeResult = tradeCache.getTrade(tradeId.toString)

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
      case Nil | null => tradeCache.getAllTrades().map{trade => trade.tradeId.toString}
      case list => list
    }

    var tradeValueCache = Map[String, TradeValuationResult]()
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

    def tradeValue(id: String): TradeValuationResult = {
      if (!tradeValueCache.contains(id))
        tradeValueCache += (id -> tradeValuer(tradeCache.getTrade(id)))
      tradeValueCache(id)
    }

    def quotaValue(id: String) = {
      tradeValue(tradeCache.tradeIDFromQuotaID(id)) match {
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

  def getTrades(tradeIds : List[String]) : List[EDMPhysicalTrade] = 
    tradeIds.map(getTrade)
  def getFuturesExchanges = futuresExchangeByGUID.values
  def getFuturesMarkets = futuresMarketByGUID.values

  /**
   * handler for events
   */
  class EventHandler extends DemultiplexerClient {
    def handle(ev: Event) {
      if (ev == null) Log.warn("Got a null event") else {
        if (Event.TradeSubject == ev.subject) { // Must be a trade event
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
              tradeIds.foreach{ id => tradeCache.removeTrade(id); tradeCache.addTrade(id)}
              val newTradeValuations = valueCostables(tradeIds, env, snapshotIDString)
              val changedIDs = tradeIds.filter{id => newTradeValuations.tradeResults(id) != originalTradeValuations.tradeResults(id)}

              if (changedIDs != Nil)
                publishChangedValueEvent(changedIDs)

              Log.info("Trades revalued for received event using snapshot %s number of changed valuations %d".format(snapshotIDString, changedIDs.size))
            }
            case NewEventVerb => {
              tradeIds.foreach(tradeCache.addTrade)
              Log.info("New event received for %s".format(tradeIds))
            }
            case CancelEventVerb | RemovedEventVerb => {
              tradeIds.foreach(tradeCache.removeTrade)
              Log.info("Cancelled / deleted event received for %s".format(tradeIds))
            }
          }
        }
      }

      // publish the valuation updated event contaning payloads of the trade id's whose trade valuations have changed
      def publishChangedValueEvent(tradeIds : List[String]) = {
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
        RabbitEvents.rabbitEventPublisher.publish(eventArray)
      }
    }
  }
}


/**
 * Titan EDM model exposed services wrappers
 */
class ValuationServiceResourceStubEx
  extends ValuationServiceResourceStub(new ValuationServiceRpc(Server.server.marketDataStore, Server.server.props, Server.server.valuationService), new java.util.ArrayList[ServiceFilter]()) {

  override def requireFilters(filterClasses: String*) {}
}

class ValuationServiceRpc(marketDataStore: MarketDataStore, val props: Props, valuationService: ValuationService) extends EdmValuationService {

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


/**
 * Rabbit glue / config module
 */
object RabbitEvents {

  val rabbitmq_host = "louis-dev-ubuntu"
  val rabbitmq_port = 5672
  val rabbitmq_username = "trafiguraDev"
  val rabbitmq_password = "trafiguraDev"
  val rabbitmq_routingKey = "RoutingKey"
  val rabbitmq_connectAttempts = 1
  val rabbitmq_virtualHost = "/"
  val rabbitmq_exchangeName = "Trafigura.Events"
  val rabbitmq_baseQueueName = "Trafigura.Events"
  val rabbitmq_durable = true
  val rabbitmq_requestedHeartbeat = 0
  val rabbitmq_requestedChannelMax = 0
  val rabbitmq_requestedFrameMax = 0
  val rabbitmq_exchangeType = "fanout"
  val rabbitmq_mandatory = false
  val rabbitmq_immediate = false
  val rabbitmq_passive = false
  val rabbitmq_exclusive = false
  val rabbitmq_publisher_autoDelete = false
  val rabbitmq_basicPropertiesDefault = new BasicProperties()

  val rabbitEventConnector = new RabbitConnector(
    rabbitmq_username,
    rabbitmq_password,
    rabbitmq_host,
    rabbitmq_port,
    rabbitmq_virtualHost,
    rabbitmq_exchangeName,
    rabbitmq_routingKey,
    rabbitmq_requestedHeartbeat,
    rabbitmq_requestedChannelMax,
    rabbitmq_requestedFrameMax)

  val rabbitEventPublisher = new RabbitPublisher(
    rabbitEventConnector,
    rabbitmq_exchangeType,
    rabbitmq_mandatory,
    rabbitmq_immediate,
    rabbitmq_passive,
    rabbitmq_durable,
    rabbitmq_publisher_autoDelete,
    rabbitmq_basicPropertiesDefault)

  val baseQueueName = ""
  val serviceName = "Starling"
  val makeQueueNameUnique = true
  val exclusive = true
  val disabled = false

  val rabbitListener = new RabbitListener(
    rabbitEventConnector,
    rabbitmq_baseQueueName + serviceName,
    makeQueueNameUnique,
    rabbitmq_passive,
    rabbitmq_durable,
    exclusive,
    rabbitmq_exclusive,
    false)

  rabbitListener.connect()

  rabbitEventPublisher.connect()

  // the demux for listener clients...
  lazy val eventDemux : EventDemultiplexer = { val demux = new EventDemultiplexer(serviceName, rabbitListener); demux.startup; demux }


  <!--

<bean id="eventDemultiplexer" class="com.trafigura.events.EventDemultiplexer" lazy-init="true" init-method="startup" destroy-method="shutdown">
      <constructor-arg index="0" value="${service.name}"/>
      <constructor-arg index="1" ref="rabbitEventListener"/>
    </bean>


  <bean id="rabbitBasicPropertiesDefault" class="com.rabbitmq.client.AMQP$BasicProperties">
    <property name="deliveryMode" value="${rabbitmq.default.basicproperties.deliveryMode}"></property>
  </bean>
  <bean id="rabbitConnectorAbstract" class="com.trafigura.services.rabbit.RabbitConnector" abstract="true"/>
  <bean id="rabbitPublisherAbstract" class="com.trafigura.services.rabbit.RabbitPublisher" abstract="true"/>
  <bean id="rabbitListenerAbstract" class="com.trafigura.services.rabbit.RabbitListener" abstract="true"/>

  <bean id="rabbitEventConnector" class="com.trafigura.services.rabbit.RabbitConnector" parent="rabbitConnectorAbstract" lazy-init="true">
    <constructor-arg value="${rabbitmq.username}"/>
    <constructor-arg value="${rabbitmq.password}"/>
    <constructor-arg value="${rabbitmq.host}"/>
    <constructor-arg value="${rabbitmq.port}"/>
    <constructor-arg value="${rabbitmq.virtualHost}"/>
    <constructor-arg value="${rabbitmq.exchangeName}"/>
    <constructor-arg value="${rabbitmq.routingKey}"/>
    <constructor-arg value="${rabbitmq.requestedHeartbeat}"/>
    <constructor-arg value="${rabbitmq.requestedChannelMax}"/>
    <constructor-arg value="${rabbitmq.requestedFrameMax}"/>
  </bean>

  <bean id="rabbitEventPublisher" class="com.trafigura.services.rabbit.RabbitPublisher" parent="rabbitPublisherAbstract" init-method="connect" destroy-method="disconnect" lazy-init="true">
    <constructor-arg ref="rabbitEventConnector"/>
    <constructor-arg value="${rabbitmq.exchangeType}"/>
    <constructor-arg value="${rabbitmq.mandatory}"/>
    <constructor-arg value="${rabbitmq.immediate}"/>
    <constructor-arg value="${rabbitmq.passive}"/>
    <constructor-arg value="${rabbitmq.durable}"/>
    <constructor-arg value="${rabbitmq.publisher.autoDelete}"/>
    <constructor-arg ref="rabbitBasicPropertiesDefault"/>
  </bean>

  <bean id="rabbitEventListener" class="com.trafigura.services.rabbit.RabbitListener" parent="rabbitListenerAbstract" init-method="connect" destroy-method="disconnect" lazy-init="true">
    <constructor-arg ref="rabbitEventConnector"/>
    <constructor-arg value="${rabbitmq.baseQueueName}.${service.name}"/>
    <constructor-arg value="${rabbitmq.makeQueueNameUnique}"/>
    <constructor-arg value="${rabbitmq.passive}"/>
    <constructor-arg value="${rabbitmq.durable}"/>
    <constructor-arg value="${rabbitmq.exclusive}"/>
    <constructor-arg value="${rabbitmq.listener.autoDelete}"/>
    <constructor-arg value="${rabbitmq.listener.disabled}"/>
  </bean> -->
}

object ValuationService extends Application {

  import org.codehaus.jettison.json.JSONObject

  println("Here")
  lazy val vs = StarlingInit.devInstance.valuationService

  vs.marketDataSnapshotIDs().foreach(println)
  val valuations = vs.valueAllQuotas()

  valuations.tradeResults.foreach(println)
   
  val (_, worked) = valuations.tradeResults.values.partition(_ match {
    case Right(_) => true
    case Left(_) => false
  })

  val valuedTradeIds = valuations.tradeResults.collect{ case (id, Left(v)) => id }.toList
  val valuedTrades = vs.getTrades(valuedTradeIds)
  val markets = vs.getFuturesMarkets.toList
  val exchanges = vs.getFuturesExchanges.toList

  writeJson("/tmp/edmTrades.json", valuedTrades)
  writeJson("/tmp/markets.json", markets)
  writeJson("/tmp/exchanges.json", exchanges)

  println("Finished")
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
}

