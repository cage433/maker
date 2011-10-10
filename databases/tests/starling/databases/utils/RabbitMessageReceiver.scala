package starling.databases.utils

import com.rabbitmq.client.QueueingConsumer.Delivery
import scala.collection.JavaConversions
import java.lang.String
import starling.gui.api.RabbitMessage
import starling.utils.{StoppableThread, ClosureUtil}
import ClosureUtil._


class RabbitMessageReceiver(host: String, queue: RabbitQueue, receiver: RabbitMessage => Unit)
  extends StoppableThread(isRunning => using (ChannelPool.openChannel(host, queue)) { channel =>
    val consumer = channel.queueingConsumer
    channel.basicConsume(queue.name, true, consumer)

    while (isRunning()) {
      val delivery: Delivery = consumer.nextDelivery(250)

      if (delivery != null && isRunning()) {
        val headers = JavaConversions.mapAsScalaMap(delivery.getProperties.getHeaders).toMap.mapValues(_.toString)
        receiver(RabbitMessage(new String(delivery.getBody), headers))
      }
    }
  }
)
