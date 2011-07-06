package starling.services.rabbit

import starling.props.Props
import com.trafigura.services.rabbit.RabbitPublisher
import com.trafigura.events.EventDemultiplexer
import com.rabbitmq.client.AMQP.BasicProperties
import com.trafigura.services.rabbit.RabbitPublisher
import com.trafigura.services.rabbit.RabbitListener
import com.trafigura.services.rabbit.RabbitConnector


/**
 * RabbitMQ event module
 */
trait RabbitEvents {

  val eventDemux : EventDemultiplexer

  val rabbitEventPublisher : RabbitPublisher

  def shutdown : Unit
}

case class DefaultRabbitEvents(props : Props) extends RabbitEvents {
  
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

  def shutdown {
    eventDemux.shutdown
    rabbitListener.disconnect()
    rabbitEventPublisher.disconnect()
  }
}

