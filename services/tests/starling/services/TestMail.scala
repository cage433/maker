package starling.services


import java.io.{ByteArrayInputStream, InputStream, ByteArrayOutputStream, BufferedInputStream}
import java.util.Properties
import javax.mail.Session
import javax.mail.internet.MimeMessage
import org.apache.commons.io.IOUtils
import org.subethamail.smtp.server.SMTPServer
import org.subethamail.smtp.{MessageContext, MessageHandler, MessageHandlerFactory}
import org.subethamail.wiser.WiserMessage

/**
 * A simple class which acts as an smtp server and prints the emails it recieves
 */

object TestMail {

  private val session = Session.getDefaultInstance(new Properties())
  class PrintMessageHandler extends MessageHandler {
    def from(from:String) {}
    def recipient(recipient:String) {}
    def data(in:InputStream) {
      val out = new ByteArrayOutputStream();
      IOUtils.copy(in, out)
      val bytes = out.toByteArray()
      val message = new MimeMessage(session, new ByteArrayInputStream(bytes));
      println( "From:    " + message.getAllRecipients.toString )
      println( "To:      " + message.getAllRecipients.toString )
      println( "Subject: " + message.getSubject)
      println
    }
    def done() { }
    def resetState() {}
    def auth(a:String,b:StringBuffer) = { true }
    def getAuthenticationMechanisms():java.util.List[String] = { java.util.Collections.emptyList[String] }
  }
  def main(args:Array[String]) {

    val server = new SMTPServer(new MessageHandlerFactory() {
      def create(context:MessageContext):MessageHandler = {
        new PrintMessageHandler()
      }
    })

    server.setPort(2500)
    server.start()
  }
}
