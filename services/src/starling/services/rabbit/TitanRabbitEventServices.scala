package starling.services.rabbit

import starling.props.Props
import com.trafigura.services.rabbit.RabbitPublisher
import com.trafigura.services.rabbit.RabbitListener
import com.trafigura.services.rabbit.RabbitConnector
import com.trafigura.common.control.PipedControl._
import com.rabbitmq.client.AMQP.BasicProperties
import com.trafigura.services.rabbit.Publisher
import org.codehaus.jettison.json.JSONArray
import com.trafigura.services.log.Logger
import com.trafigura.events._
import starling.gui.api.MarketDataSnapshot
import starling.utils.{TypedBroadcaster, Log, Stoppable}
import com.trafigura.shared.events._


/**
 * Titan RabbitMQ event module
 */
trait TitanRabbitEventServices extends Stoppable with Logger {
  val eventDemux : IDemultiplexEvents
  val rabbitEventPublisher : Publisher
  def addClient(client:DemultiplexerClient)
}

case class TitanRabbitIdBroadcaster(
    publisher : Publisher,
    source : String = Event.StarlingSource,
    subject : String = Event.StarlingMarketDataSnapshotIDSubject,
    verb : EventVerbEnum = NewEventVerb,
    payloadType : String = Event.StarlingSnapshotIdPayload) extends TypedBroadcaster[MarketDataSnapshot] with Logger {

  def typedBroadcast(mds: MarketDataSnapshot) = try {
    val events = createUpdatedIDEvents(mds.snapshotIDs, source, subject, verb, payloadType)
    publisher.publish(events)
  }
  catch { // catch and log all exceptions since we don't want to cause the event broadcaster to fail more generally
    case th : Throwable => Log.error("Caught exception in Titan Rabbit event broadcaster %s".format(th.getMessage))
  }

  private def createUpdatedIDEvents(ids : List[String], source : String, subject : String, verb : EventVerbEnum, payloadType : String) : JSONArray = {
    val pf = new PayloadFactory()
    val payloads = ids.map(id => pf.createPayload(payloadType, source, id))
    createEvents(source, subject, verb, payloads)
  }

  private def createEvents(source : String, subject : String, verb : EventVerbEnum, payloads : List[Payload]) : JSONArray = {
    val keyIdentifier = System.currentTimeMillis.toString
    val ev = EventFactory().createEvent(subject, verb, source, keyIdentifier, payloads)
    ||> { new JSONArray } { r => r.put(ev.toJson) }
  }
}

object EventFactory {
  lazy val ef = new EventFactory()
  def apply() : EventFactory = ef
}
object EventPayloadFactory {
  lazy val pf = new PayloadFactory()
  def apply() : PayloadFactory = pf
}

case class DefaultTitanRabbitEventServices(props : Props) extends TitanRabbitEventServices {
  
  val rabbitmq_host = props.TitanRabbitBrokerHost()
  val rabbitmq_port = 5672
  val rabbitmq_username = props.TitanRabbitUserName()
  val rabbitmq_password = props.TitanRabbitPassword()
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
  val rabbitmq_exclusive = true
  val rabbitmq_publisher_autoDelete = false
  val rabbitmq_basicPropertiesDefault = new BasicProperties()
  val rabbitmq_makeQueueNameUnique = true

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

  val rabbitListener = new RabbitListener(
    rabbitEventConnector,
    rabbitmq_baseQueueName + serviceName,
    rabbitmq_makeQueueNameUnique,
    rabbitmq_passive,
    rabbitmq_durable,
    rabbitmq_exclusive,
    rabbitmq_publisher_autoDelete)

  // the demux for listener clients...
  lazy val eventDemux : EventDemultiplexer = ||> { new EventDemultiplexer(serviceName, rabbitListener)} { r => r.startup }

  val clients = new scala.collection.mutable.ArrayBuffer[DemultiplexerClient]()

  def addClient(client:DemultiplexerClient) {
    clients.append(client)
    if (isRunning) {
      eventDemux.addClient(client)
    }
  }

  override def start {
    super.start
    rabbitListener.connect()
    rabbitEventPublisher.connect()
    clients.foreach(client => eventDemux.addClient(client))
  }

  override def stop {
    super.start
    eventDemux.shutdown
    rabbitListener.disconnect()
    rabbitEventPublisher.disconnect()
  }
}


/**
 * Mocked event handlers for testing
 */
case class MockTitanRabbitEventServices() extends TitanRabbitEventServices {
  private val mockDemux = new MockEventDemux()
  val eventDemux : IDemultiplexEvents = mockDemux
  val rabbitEventPublisher = new MockRabbitPublisher(mockDemux)
  def addClient(client:DemultiplexerClient)  { eventDemux.addClient(client) }
}

class MockRabbitPublisher(val eventDemux : MockEventDemux) extends Publisher {
  def publish(payload: JSONArray) = {
    for (i <- 0 until payload.length()) {
      val obj = payload.getJSONObject(i)
      eventDemux.publishToClients(Event.fromJson(obj))
    }
  }
}

class MockEventDemux() extends IDemultiplexEvents {
  private var clients = List[DemultiplexerClient]()  
  def startup {}
  def shutdown {}

  def addClient(client : DemultiplexerClient) : Unit = synchronized {
    clients = client :: clients
  }

  private def dispatchToClients(ev : Event) : Unit = {
    val theClients = this.synchronized {
       clients
    }
    for (client <- theClients) {
      try {
        client.handle(ev)
      } catch {
        case e => {
          val msg = " event demultiplexer client " + client + " encountered exception"
          println(msg + ", " + e.getMessage + "\n" + e.getStackTrace.mkString(","))
          Log.warn(msg, e)
        }
      }
    }    
  }
  def publishToClients(ev : Event) { dispatchToClients(ev) }
  def getOriginatingHosts = "mock"
  def getName = "mock"
  def getReceived = 0
  def getProcessed = 0
  def resetStatistics {}
}

/*
class MockRabbitEventPublisher() extends EventPublisher {
  this(eventDemux : MockEventDemux)
  val demux = eventDemux

  // builds and returns a payload
  def newPayload(payloadType: String, id : Int) : Payload = null
  def newPayload(payloadType: String, id : String) : Payload = null
  def publish(subject : String, verb : EventVerbEnum) {}
  def publishNew(subject : String, payload : Payload) {}
  def publishUpdate(subject : String, payload : Payload) {}
  def publish(subject : String, verb : EventVerbEnum, payload : Payload) = null
  def publish(subject : String, verb : EventVerbEnum, payloads : List[Payload]) = null
  def getPublished : Int = 0
  def resetStatistics {}
  def getPublisher : String = "Mock"
}
*/

