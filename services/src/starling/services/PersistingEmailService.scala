package starling.services

import starling.db.DB
import starling.daterange.Timestamp
import starling.pivot.model.PivotTableModel
import starling.dbx.{From, RealTable}
import starling.pivot._
import starling.utils.ImplicitConversions._
import org.springframework.mail.javamail.{JavaMailSenderImpl, MimeMessageHelper, JavaMailSender}
import org.springframework.dao.DuplicateKeyException
import starling.utils.{Broadcaster, Log}
import starling.gui.api.{EmailSent, Email}

class PersistingEmailService(broadcaster: Broadcaster, db: DB, jSender: JavaMailSender) extends EmailService with Log {
  def this(broadcaster: Broadcaster, db: DB, host: String, port: Int) =
    this(broadcaster, db, new JavaMailSenderImpl().update(_.setHost(host), _.setPort(port)))

  def send(email: Email) = db.inTransaction { writer => new Timestamp().update { now =>
    val sendEmail = try writer.insert("EmailsSent", Map(
      "hash"      → email.hash,
      "timestamp" → now,
      "sender"    → email.from,
      "recipient" → email.to,
      "subject"   → email.subject,
      "body"      → email.bodyWithFooters
    ) ) catch {
      case e: DuplicateKeyException => false // Email already sent
      case _ => true                         // DB is broken somehow, but sending is more important
    }

    if (sendEmail) {
      jSender.send(toMessage(email))
      broadcaster.broadcast(EmailSent(now))
    }
  } }

  def emailsSent(mostRecent: Timestamp, pivotFieldParams: PivotFieldParams) = PivotTableModel.createPivotData(
    new OnTheFlySQLPivotTableDataSource(db,
      List(("Email Fields", List(new TimestampAsDayColumnDefinition("Day", "EmailsSent"),
        new TimestampAsTimeColumnDefinition("Time", "timestamp", "EmailsSent")) :::
        stringColumns("subject", "recipient", "sender", "body")
      )),
      From(RealTable("EmailsSent"), Nil), Nil, PivotFieldsState(rowFields = List(Field("Day"), Field("Time"), Field("Subject")),
        dataFields = List(Field("Sender"), Field("Recipient"), Field("Body"))), Nil
    ), pivotFieldParams
  )

  private def toMessage(email: Email) = jSender.createMimeMessage.update { message => new MimeMessageHelper(message, true).update(
    _.setFrom(email.from), _.setTo(email.to), _.setSubject(email.subject), _.setText(email.bodyWithFooters, true)
  ) }

  private def stringColumns(columns: String*) = columns.map(c => StringColumnDefinition(c.capitalize, c, "EmailsSent")).toList
}

trait FooterAppendingEmailService extends EmailService {
  abstract override def send(message: Email) = super.send(message + footer)

  def footer: Map[String, String]
}