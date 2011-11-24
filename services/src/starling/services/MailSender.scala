package starling.services

import starling.utils.ImplicitConversions._
import starling.gui.api.Email
import starling.props.Props
import java.net.Socket
import java.io.{DataInputStream, DataOutputStream}
import starling.utils.ClosureUtil._
import org.springframework.mail.javamail.{JavaMailSenderImpl, MimeMessageHelper}
import starling.utils.Log

trait MailSender {
  def send(email: Email): Boolean
}

class JavaMailMailSender(smtpServer: String, port: Int) extends MailSender {
  private val jSender = new JavaMailSenderImpl().update(_.setHost(smtpServer), _.setPort(port))

  def send(email: Email): Boolean = succeeds(jSender.send(toMessage(email)))

  private def toMessage(email: Email) = jSender.createMimeMessage.update { message => new MimeMessageHelper(message, false).update(
    _.setFrom(email.from), _.setTo(email.to.toArray), _.setSubject(email.subject), _.setText(email.bodyWithFooters, true)
  ) }
}

class SMTPMailSender(serverName: String, smtpServer: String, port: Int) extends MailSender with Log {
  def this(props: Props) = this(props.ExternalHostname(), props.SmtpServerHost(), props.SmtpServerPort())

  def send(email: Email): Boolean = {
    val socket = new Socket(smtpServer, port)
    val (in, out) = (new DataInputStream(socket.getInputStream), new DataOutputStream(socket.getOutputStream))
    val commands = commandsFor(email)

    if (socket == null || in == null || out == null) false else try {
      commands.foreach(command => out.writeBytes(command + "\r\n"))

      val lines = readLines(in)

      in.close; out.close; socket.close

      val mailSent = lines.exists(_.contains("Queued mail for delivery"))

      if (!mailSent) {
        log.error("Failed to send email, commands: \n%s\nresponse:\n%s\n" % (commands.mkString("\n"), lines.mkString("\n")))
      }

      mailSent
    } catch {
      case e => log.error("Failed to send email, commands: \n%s" % (commands.mkString("\n"))); throw e
    }
  }

  private def commandsFor(email: Email): List[String] = List(
    "HELO %s" % serverName,
    "MAIL FROM:<%s>" % email.from
  ) ++ email.to.map("RCPT TO:<%s>" % _) ++ List(
    "DATA",
    "From: <%s>" % email.from
  ) ++ email.to.map("To: <%s>" % _) ++ List(
    "MIME-Version: 1.0",
    "Content-Type: text/html",
    "Subject: %s" % email.subject,
    "", email.bodyWithFooters,
    "\r\n.", "QUIT"
  )

  private def readLines(in: DataInputStream): Stream[String] = {
    def readLine = try { in.readLine } catch { case _ => null }

    Stream.continually(readLine).takeWhile(_ != null)
  }
}
