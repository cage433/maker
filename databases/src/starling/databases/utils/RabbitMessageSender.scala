package starling.databases.utils

import com.rabbitmq.client._
import scala.collection.JavaConversions
import starling.gui.api.RabbitMessage
import starling.utils.ClosureUtil._

class RabbitMessageSender(host: String) {
  def send(queueName: String, message: RabbitMessage) =
    using (ChannelPool.openChannel(host, RabbitQueue(queueName))) { channel =>
      val props = new AMQP.BasicProperties();
      props.setHeaders(JavaConversions.mapAsJavaMap(message.headers))

      channel.basicPublish("", queueName, props, message.body.getBytes)
    }
}

