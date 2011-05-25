package starling.utils

import com.rabbitmq.client._
import scala.collection.JavaConversions
import starling.gui.api.RabbitMessage
import ClosureUtil._

class RabbitMessageSender(host : String) {
  def send(queueName : String, message : RabbitMessage) : Unit =
    using (ChannelPool.openChannel(host, RabbitQueue(queueName, Durable))) { channel =>
      val props = new AMQP.BasicProperties();
        props.setHeaders(JavaConversions.asJavaMap(message.headers))

        channel.basicPublish("", queueName, props, message.body.getBytes)
    }
}

