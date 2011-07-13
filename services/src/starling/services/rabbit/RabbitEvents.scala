package starling.services.rabbit

import starling.props.Props
import com.trafigura.services.rabbit.RabbitPublisher
import com.trafigura.events.EventDemultiplexer
import com.trafigura.services.rabbit.RabbitPublisher
import com.trafigura.services.rabbit.RabbitListener
import com.trafigura.services.rabbit.RabbitConnector
import com.trafigura.common.control.PipedControl._
import com.rabbitmq.client.AMQP.BasicProperties
import com.trafigura.services.rabbit.Publisher
import com.trafigura.events.EventPublisher
import com.trafigura.events.IDemultiplexEvents
import com.trafigura.shared.events.Event
import com.trafigura.events.DemultiplexerClient
import org.codehaus.jettison.json.JSONArray
import starling.utils.Stoppable


/**
 * RabbitMQ event module
 */
trait RabbitEventServices extends Stoppable {
  val eventDemux : IDemultiplexEvents
  val rabbitEventPublisher : Publisher
}

case class DefaultRabbitEventServices(props : Props) extends RabbitEventServices {
  
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

  def start {
    rabbitListener.connect()
    rabbitEventPublisher.connect()
  }

  def stop {
    eventDemux.shutdown
    rabbitListener.disconnect()
    rabbitEventPublisher.disconnect()
  }
}


/**
 * Mocked event handlers for testing
 */
case class MockRabbitEventServices() extends RabbitEventServices {
  private val mockDemux = new MockEventDemux()
  val eventDemux : IDemultiplexEvents = mockDemux
  val rabbitEventPublisher = new MockRabbitPublisher(mockDemux)
  def start {}
  def stop {}
}

class MockRabbitPublisher(val eventDemux : MockEventDemux) extends Publisher {
  //this(eventDemux : MockEventDemux)
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
    clients ++= List(client)
  }

  private def dispatchToClients(ev : Event) : Unit = {
    val theClients = this.synchronized {
       clients
    }
    for (client <- theClients) {
      try {
        client.handle(ev)
      } catch {
        case e => //Log.warn(name + " event demultiplexer client " + client + " encountered exception", e)
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

