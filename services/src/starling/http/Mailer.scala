package starling.http

import com.sun.xml.internal.messaging.saaj.packaging.mime.internet.MimeBodyPart
import javax.activation.{FileDataSource, DataHandler}
import javax.mail.internet.{MimeMessage, MimeMultipart, InternetAddress}
import javax.mail.{Transport, Session, Message}

class Mailer(smtpHost:String) {
  private val session = {
    val props = System.getProperties(); //change
    props.put("mail.smtp.host", smtpHost);
    Session.getInstance(props, null)
  }

  class Attachment(name:String, contentType:String, data:Array[Byte])

  class Message(from:String, to:String, subject:String, body:String, attachments:List[Attachment]) {
    def this(from:String, to:String, subject:String) = this(from, to, subject, "", List())

    def body(newBody:String) = new Message(from, to, subject, newBody, attachments)

    def attachment(name:String, contentType:String, data:Array[Byte]) = {
      new Message(from, to, subject, body, attachments ::: List(new Attachment(name, contentType, data)))
    }

    def send() {
//      val msg = new MimeMessage(session)
//      msg.setFrom(new InternetAddress(from))
//      val address= {new InternetAddress(to)}
//      msg.setRecipients(Message.RecipientType.TO, to)
//      msg.setSubject(subject)
//      msg.setSentDate(new java.util.Date());
//
//      val mp = new MimeMultipart();
//
//      val mainBodyPart = new MimeBodyPart()
//      mainBodyPart.setText(body)
//      mp.addBodyPart(mainBodyPart)

//      attachments.foreach( (attachement) => {
//        val mbp2 = new MimeBodyPart()
//        val fds = new FileDataSource(filename)
//        mbp2.setDataHandler(new DataHandler(fds))
//        mbp2.setFileName(fds.getName())
//        mp.addBodyPart(mainBodyPart)
//      })

//      msg.setContent(mp);
//      Transport.send(msg);
    }
  }
}