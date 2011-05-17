package starling.utils

import java.util.concurrent.{TimeUnit, LinkedBlockingQueue}

import org.scalatest.matchers.ShouldMatchers
import collection.immutable.Map
import org.testng.annotations.Test
import starling.gui.api.RabbitMessage
import ClosureUtil._
import util.Random

class RabbitTest extends StarlingTest with ShouldMatchers {
  @Test(enabled=false, description="Unable to test until it's possible to run a rabbitmq server from within the test")
  def canSendAndReceiveMessagesOverRabbitMQ {
    val host = "localhost" //tradinghub-test"
    val rabbitQueue = RabbitQueue("Trafigura.Raw.Trade.RiskManagement", Durable)
    // val rabbitQueue = RabbitQueue("Trafigura.TradeCSV.RiskManagement", Durable)

    using (new QueuedRabbitMessageReceiver(host, rabbitQueue)) { receiver =>
      receiver.start

      val message = RabbitMessage("Hello world " + new Random(System.currentTimeMillis).nextInt, Map("header" -> "value"))

      new RabbitMessageSender(host).send(rabbitQueue.name, message)

      val result = receiver.messages.poll(1, TimeUnit.SECONDS)

      result should not be (null)
      result should equal (message)
    }
  }

  @Test(enabled=false)
  def drainQueue {
    val host = "localhost"
    //val host = "tradinghub-test"
    val rabbitQueue = RabbitQueue("Trafigura.Raw.Trade.RiskManagement", Durable)
    //val rabbitQueue = RabbitQueue("Trafigura.TradeCSV.RiskManagement", Durable)

    using (new QueuedRabbitMessageReceiver(host, rabbitQueue)) { receiver =>
      var result = receiver.messages.poll(1, TimeUnit.SECONDS)

      while (result != null) {
        Console.println(result.body)

        result = receiver.messages.poll(1, TimeUnit.SECONDS)
      }
    }
  }
}

class QueuedRabbitMessageReceiver(host : String, queue : RabbitQueue) extends Stoppable {
  private val receiver = new RabbitMessageReceiver(host, queue, message => { messages.add(message)})
  val messages = new LinkedBlockingQueue[RabbitMessage]

  def start = receiver.start
  def stop = receiver.stop
}

