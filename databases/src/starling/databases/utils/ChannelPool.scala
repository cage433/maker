package starling.databases.utils

import com.rabbitmq.client._

import starling.utils.ClosureUtil
import java.io.IOException

object ConnectionPool {
  def openConnection(host : String) = {
    val factory = new ConnectionFactory()

    val connection = factory.newConnection(host)

    if (!connection.isOpen) {
      throw new IOException("Could not open connection to host: " + host)
    }

    connection
  }
}

object ChannelPool {
  val DeclareQueuesPassively = true
  val IsDurable = true
  val NonExclusive = false
  val DoNotAutoDelete = false

  def openChannel(host: String, queue: RabbitQueue): RabbitChannel = {
    val connection: Connection = ConnectionPool.openConnection(host)
    var channel: Channel = null

    try {
      channel = connection.createChannel
      channel.queueDeclare(queue.name, DeclareQueuesPassively, IsDurable, NonExclusive, DoNotAutoDelete, null)
    } catch {
      case e : Exception => {
        if (channel != null && channel.isOpen) channel.close
        if (connection.isOpen) connection.close

        throw new IOException("Unable to declare queue", e)
      }
    }

    new RabbitChannel(channel, connection, queue)
  }
}

class RabbitChannel(channel: Channel, connection: Connection, queue: RabbitQueue) {
  def close() { channel.close; connection.close }

  def basicPublish(exchange: String, routingKey: String, props: AMQP.BasicProperties, body: Array[Byte]) {
    channel.basicPublish(exchange, routingKey, props, body)
  }

  def basicConsume(queue: String, noAck: Boolean, callback: Consumer) = channel.basicConsume(queue, noAck, callback)
  def queueingConsumer = new QueueingConsumer(channel)
}

case class RabbitQueue(name: String)