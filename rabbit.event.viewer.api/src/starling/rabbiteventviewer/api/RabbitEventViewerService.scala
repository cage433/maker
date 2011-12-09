package starling.rabbiteventviewer.api

import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.manager.Memoize

trait RabbitEventViewerService {
  @Memoize def rabbitEvents(pivotFieldParams:PivotFieldParams, latestID:Long):PivotData
  def latestRabbitEvent:Long
}