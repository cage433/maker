package starling.utils

import com.rabbitmq.client._
import java.lang.String
import scala.collection.JavaConversions
import starling.utils.ImplicitConversions._


class SampleTradeCapturer extends Application {
  var factory = new ConnectionFactory()

  val conn = factory.newConnection("tradinghub-test")
  var chan = conn.createChannel

  val doNotCreateQueue: Boolean = true
  val durable: Boolean = true
  val nonExclusive: Boolean = false
  val doNotAutoDelete: Boolean = false
  val noAck: Boolean = true

  chan.queueDeclare("Trafigura.TradeCSV.RiskManagement", doNotCreateQueue, durable, nonExclusive, doNotAutoDelete, null)

  System.out.println(" [*] Waiting for messages. To exit press CTRL+C")
  var consumer = new QueueingConsumer(chan)
  chan.basicConsume("Trafigura.TradeCSV.RiskManagement", noAck, consumer)

  while (true) {
    val delivery = consumer.nextDelivery
    val headers = JavaConversions.asScalaMap(delivery.getProperties.getHeaders)
    val header = (name : String) => headers.get(name).toString

    System.out.println(" [TradeCSv] Received (userName = %s, subGroupName = %s), data = %s" %
      (header("userName"), header("subGroupName"), new String(delivery.getBody)))
  }
}

