package starling.services

import starling.pivot._
import starling.daterange.Day
import starling.rmi.TrinityUploader
import starling.gui.api._
import starling.curves.ClosesEnvironmentRule


class UploadCurveToTrinityTask(uploader: TrinityUploader, marketDataIdentifier: => MarketDataIdentifier) extends ScheduledTask {
  def execute(observationDay: Day) = {
    uploader.uploadCurve(CurveLabel(CurveTypeLabel("Price"), marketDataIdentifier,
      EnvironmentSpecificationLabel(observationDay, ClosesEnvironmentRule.label)))
  }
}

class UploadLiborToTrinityTask(uploader: TrinityUploader) extends ScheduledTask {
  def execute(observationDay: Day) = {
    uploader.uploadLibor(observationDay)
  }
}
