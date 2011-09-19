package starling.rabbiteventviewer.api

import starling.pivot.PivotFieldParams
import starling.rmi.PivotData

trait RabbitEventViewerService {
  def rabbitEvents(pivotFieldParams:PivotFieldParams, latestID:Long):PivotData
  def latestRabbitEvent:Long
}