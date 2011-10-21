package starling.services

import starling.gui.api.Email
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.daterange.Timestamp


trait EmailService {
  def send(message: Email)
  def emailsSent(mostRecent: Timestamp, pivotFieldParams: PivotFieldParams): PivotData
}