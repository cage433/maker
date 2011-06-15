package starling.services

import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSender}

import starling.gui.api.EmailEvent
import starling.props.{PropsHelper, Props}
import starling.utils.TypedBroadcaster

import starling.utils.ImplicitConversions._


class EmailBroadcaster(sender: JavaMailSender, props: Props = PropsHelper.defaultProps) extends TypedBroadcaster[EmailEvent] {
  val footer = "\n\n<br /><br /><i>" + "Sent by: %s, name: %s, host: %s</i>" %
    (props.ServerType(), props.ServerName(), props.ExternalHostname())

  def typedBroadcast(email: EmailEvent) {
    val message = sender.createMimeMessage
    val helper = new MimeMessageHelper(message, true)
    helper.setFrom(email.from)
    helper.setTo(email.to)
    helper.setSubject(email.subject)
    helper.setText(email.body + footer, true)

    sender.send(message)
  }
}