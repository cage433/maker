package rabbitmq

import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.Connection
import com.rabbitmq.client.Channel

object Send {
  def main(argv:Array[String]) {
    val factory: ConnectionFactory = new ConnectionFactory
    factory.setHost("localhost")

    val conn: Connection = factory.newConnection
    val chan: Channel = conn.createChannel

    chan.queueDeclare("hello", false, false, false, null)
    chan.basicPublish("", "hello", null, "Hello World!".getBytes)

    System.out.println(" [x] Sent 'Hello World!'")

    chan.close
    conn.close
  }
}