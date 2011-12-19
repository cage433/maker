package starling.metals.tasks

import starling.daterange.Day
import starling.gui.api._
import starling.curves.AllClosesEnvironmentRule
import starling.scheduler.{ScheduledTaskAttribute, ScheduledTask}


class UploadCurveToTrinityTask(uploader: TrinityUploader, marketDataIdentifier: => MarketDataIdentifier) extends ScheduledTask {
  override def attributes = super.attributes + DataSink → ScheduledTaskAttribute("Trinity")

  def execute(observationDay: Day) = uploader.uploadCurve(CurveLabel(CurveTypeLabel("Price"), marketDataIdentifier,
    EnvironmentSpecificationLabel(observationDay.endOfDay, AllClosesEnvironmentRule.label())))
}
