package starling.services

import starling.utils.TypedBroadcaster
import starling.gui.api.EmailEvent
import org.springframework.mail.javamail.{MimeMessageHelper, JavaMailSender}


class EmailBroadcaster(sender: JavaMailSender) extends TypedBroadcaster[EmailEvent] {
  def typedBroadcast(email: EmailEvent) {
    val message = sender.createMimeMessage
    val helper = new MimeMessageHelper(message, true)
    helper.setFrom(email.from)
    helper.setTo(email.to.toArray)
    helper.setSubject(email.subject)
    helper.setText(email.body, true)

    sender.send(message)
  }
}