package starling.services

import starling.gui.api.Email
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.daterange.Timestamp
import scalaz.Scalaz._

trait EmailService {
  def send(message: Email)
  def emailsSent(mostRecent: Timestamp, pivotFieldParams: PivotFieldParams): PivotData
  def enabledIf(condition: Boolean): EmailService = condition ? this | new DisabledEmailService(this)
}

class DisabledEmailService(adapted: EmailService) extends EmailService {
  def send(message: Email) {}
  def emailsSent(mostRecent: Timestamp, pivotFieldParams: PivotFieldParams) = adapted.emailsSent(mostRecent, pivotFieldParams)
  override def enabledIf(condition: Boolean) = this
}