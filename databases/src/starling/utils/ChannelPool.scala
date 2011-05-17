package starling.utils

import com.rabbitmq.client._

import ClosureUtil._
import java.io.IOException

object ConnectionPool {
  def openConnection(host : String) = {
    val factory = new ConnectionFactory
    factory.setHost(host)

    val connection = factory.newConnection

    if (!connection.isOpen) {
      throw new IOException("Could not open connection to host: " + host)
    }

    connection
  }
}

object ChannelPool {
  val DeclareQueuesPassively = true
  val NonExclusive = false
  val DoNotAutoDelete = false

  def openChannel(host : String, queue : RabbitQueue) : Bound[Channel] = {
    val connection = ConnectionPool.openConnection(host)
    var channel: Channel = null

    try {
      channel = connection.createChannel
      channel.queueDeclare(queue.name, DeclareQueuesPassively, queue.isDurable, NonExclusive, DoNotAutoDelete, null)
    } catch {
      case e : Exception => {
        if (channel != null && channel.isOpen) channel.close
        if (connection.isOpen) connection.close

        throw new IOException("Unable to declare queue", e)
      }
    }

    Bound(channel, connection)
  }
}

case class RabbitQueue(name: String, durability : QueueDurability) {
  def isDurable = durability match {
    case Durable => true
    case _ => false
  }
}

sealed abstract class QueueDurability
case object Durable extends QueueDurability
case object Transient extends QueueDurability