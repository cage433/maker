package starling.databases.utils

import starling.utils.TypedBroadcaster
import starling.gui.api.RabbitEvent
import starling.utils.ClosureUtil._

class RabbitBroadcaster(sender: RabbitMessageSender) extends TypedBroadcaster[RabbitEvent] {
  def typedBroadcast(rabbitEvent: RabbitEvent) = safely { sender.send(rabbitEvent.queueName, rabbitEvent.toMessage) }
}
