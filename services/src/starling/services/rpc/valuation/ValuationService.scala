package starling.services.rpc.valuation

import starling.props.Props
import starling.instrument.PhysicalMetalForward
import starling.daterange.Day
import starling.db.{NormalMarketDataReader, SnapshotID, MarketDataStore}
import starling.gui.api.MarketDataIdentifier._
import starling.curves.{ClosesEnvironmentRule, Environment}
import starling.gui.api.{MarketDataIdentifier, PricingGroup}
import org.joda.time.LocalDate
import starling.utils.{Log, Stopwatch}
import com.trafigura.shared.events.{UpdatedEventVerb, NewEventVerb, Event}
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
import javax.management.remote.rmi._RMIConnection_Stub
import java.lang.Exception

/**
 * Valuation service implementations
 */
class ValuationService(marketDataStore: MarketDataStore, val props: Props) extends TacticalRefData(props: Props) with ValuationServiceApi {


  private var tradeMap: Map[String, EDMPhysicalTrade] = Map[String, EDMPhysicalTrade]()
  private var quotaIDToTradeIDMap: Map[String, String] = Map[String, String]()


  private def updateTradeMap() {
    val sw = new Stopwatch()
    val edmTradeResult = titanGetEdmTradesService.getAll()
    Log.info("Are EDM Trades available " + edmTradeResult.cached + ", took " + sw)
    if (!edmTradeResult.cached) throw new TradeManagementCacheNotReady
    Log.info("Got Edm Trade results " + edmTradeResult.cached + ", trade result count = " + edmTradeResult.results.size)
    tradeMap = edmTradeResult.results.map(_.trade.asInstanceOf[EDMPhysicalTrade]).filter(pt => pt.tstate == CompletedTradeTstate).map(t => (t.tradeId.toString, t)).toMap
    quotaIDToTradeIDMap = tradeMap.flatMap{
      case (tradeID, trade) => trade.quotas.map{quota => (tradeID, quota.detail.identifier)}
    }.toMap
  }

  private def getAllTrades(): List[EDMPhysicalTrade] = {
    if (tradeMap.size > 0) {
      tradeMap.values.toList
    }
    else {
      updateTradeMap()
      tradeMap.values.toList
    }
  }

  private def getTrade(id: String): EDMPhysicalTrade = {
    if (tradeMap.contains(id)) {
      tradeMap(id)
    }
    else {
      val trade = titanGetEdmTradesService.getByOid(id.toInt)
      tradeMap += trade.tradeId.toString -> trade.asInstanceOf[EDMPhysicalTrade]
      tradeMap(id)
    }
  }



  /**
   * value all edm trade quotas (that are completed) and return a structure containing a
   *   map from tradeId to either a list of CostsAndIncomeQuotaValuation or strings
   */
  def valueAllQuotas(maybeSnapshotIdentifier: Option[String] = None): CostsAndIncomeQuotaValuationServiceResults = {
    log("valueAllQuotas called with snapshot id " + maybeSnapshotIdentifier)
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val sw = new Stopwatch()
    val edmTrades = getAllTrades()
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
  def valueTradeQuotas(tradeId: Int, maybeSnapshotIdentifier: Option[String] = None): (String, Either[List[CostsAndIncomeQuotaValuation], String]) = {
    log("valueTradeQuotas called for trade %d with snapshot id %s".format(tradeId, maybeSnapshotIdentifier))
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)

    val sw = new Stopwatch()

    val edmTradeResult = titanGetEdmTradesService.getByOid(tradeId)

    log("Got Edm Trade result " + edmTradeResult)
    val env = environment(snapshotStringToID(snapshotIDString))
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _

    val edmTrade: EDMPhysicalTrade = edmTradeResult.asInstanceOf[EDMPhysicalTrade]
    log("Got %d physical trade".format(edmTrade))
    sw.reset()
    val valuation = tradeValuer(edmTrade)
    log("Valuation took " + sw)

    (snapshotIDString, valuation)
  }

  def tradeIDFromQuotaID(quotaID: String): String = {
    if (!quotaIDToTradeIDMap.contains(quotaID))
      updateTradeMap()
    quotaIDToTradeIDMap.get(quotaID) match {
      case Some(tradeID) => tradeID
      case None => throw new Exception("Missing quota " + quotaID)
    }
  }

  /**
   * value all costables by id
   */
  def valueCostables(costableIds: List[String], maybeSnapshotIdentifier: Option[String]): CostsAndIncomeQuotaValuationServiceResults = {

    val sw = new Stopwatch()

    val idsToUse = costableIds match {
      case Nil => {updateTradeMap(); tradeMap.keys.toList}
      case list => list
    }

    type TradeValuationResult = Either[List[CostsAndIncomeQuotaValuation], String]
    var tradeValueCache = Map[String, TradeValuationResult]()
    val snapshotIDString = resolveSnapshotIdString(maybeSnapshotIdentifier)
    val env = environment(snapshotStringToID(snapshotIDString))
    val tradeValuer = PhysicalMetalForward.value(futuresExchangeByGUID, futuresMarketByGUID, env, snapshotIDString) _
    def tradeValue(id: String): TradeValuationResult = {
      if (!tradeValueCache.contains(id))
        tradeValueCache += (id -> tradeValuer(getTrade(id)))
      tradeValueCache(id)
    }

    def quotaValue(id: String) = {
      tradeValue(tradeIDFromQuotaID(id)) match {
        case Left(list) => Left(list.filter(_.quotaID == id))
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
    val tradeValues = tradeIDs.map {
      case id =>
        (id, tradeValue(id))
    }
    val quotaValues = quotaIDs.map {
      case id =>
        (id, quotaValue(id))
    }

    log("Valuation took " + sw)
    val (errors, worked) = (tradeValues ::: quotaValues).partition(_._2 match {
      case Right(_) => true
      case Left(_) => false
    })
    log("Worked " + worked.size + ", failed " + errors.size + ", took " + sw)

    CostsAndIncomeQuotaValuationServiceResults(snapshotIDString, (tradeValues ::: quotaValues).take(5440).toMap)

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

  private def environment(snapshot: SnapshotID): Environment = {
    val reader = new NormalMarketDataReader(marketDataStore, MarketDataIdentifier(snapshot.marketDataSelection, snapshot.version))
    ClosesEnvironmentRule.createEnv(snapshot.observationDay, reader).environment
  }


  /**
   * handler for events
   */
  class EventHandler extends DemultiplexerClient {
    def handle(ev: Event) {
      require(ev != null, "Got a null event ?!")
      if ((Event.TradeSubject == ev.subject) && // Must be a trade event
        (UpdatedEventVerb == ev.verb)) {
        Log.info("handler: Got a trade event to process %s".format(ev.toString))
        val tradeValuation = valueTradeQuotas(1)
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

    val valuationResult = valuationService.valueAllQuotas(if (maybeSnapshotIdentifier  == null) None else Some(maybeSnapshotIdentifier))

    Log.info("got valuationResult, size %d".format(valuationResult.tradeResults.size))

    valuationResult
  }

  def valueCostables(costableIds: List[String], maybeSnapshotIdentifier: String): EdmCostsAndIncomeQuotaValuationServiceResults = {

    valuationService.valueCostables(costableIds, if (maybeSnapshotIdentifier == null) None else Some(maybeSnapshotIdentifier))
  }
}


/**
 * Rabbit glue / config module
 */
object Rabbit {

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

  //rabbitListener.connect()
  //rabbitEventPublisher.connect()

  // the demux for listener clients...
  //lazy val eventDemux = new EventDemultiplexer(serviceName, rabbitListener)


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
  lazy val vs = StarlingInit.devInstance.valuationService

  vs.marketDataSnapshotIDs().foreach(println)
  val valuations = vs.valueAllQuotas()
}
