package starling.services

import starling.gui.api.EmailEvent
import starling.props.Props
import starling.utils.TypedBroadcaster


class EmailBroadcaster(sender: EmailService, props: Props) extends TypedBroadcaster[EmailEvent] {
  private val footer = Map("Sent by" → props.ServerType(), "Name" → props.ServerName(), "Host" → props.ExternalHostname())

  def typedBroadcast(event: EmailEvent) = sender.send(event.email + footer)
}