package starling.rabbiteventviewer.api

import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.manager.DoNotCache

trait RabbitEventViewerService {
  def rabbitEvents(pivotFieldParams:PivotFieldParams, latestID:Long):PivotData
  @DoNotCache def latestRabbitEvent:Long
}