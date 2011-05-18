package starling.services

import starling.utils.TypedBroadcaster
import starling.gui.api.EmailEvent
import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSender}
import starling.props.{PropsHelper, Props}

class EmailBroadcaster(sender: JavaMailSender, props: Props = PropsHelper.defaultProps) extends TypedBroadcaster[EmailEvent] {
  val footer = "\n\n<br /><br /><i>" + "Sent by " +  props.ServerName() + ", host: " + props.ExternalHostname() + "</i>"

  def typedBroadcast(email: EmailEvent) {
    val message = sender.createMimeMessage
    val helper = new MimeMessageHelper(message, true)
    helper.setFrom(email.from)
    helper.setTo(email.to.toArray)
    helper.setSubject(email.subject)
    helper.setText(email.body + footer, true)

    sender.send(message)
  }
}