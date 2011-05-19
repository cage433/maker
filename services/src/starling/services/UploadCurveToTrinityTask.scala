package starling.services

import starling.daterange.Day
import starling.rmi.TrinityUploader
import starling.gui.api._
import starling.curves.ClosesEnvironmentRule


class UploadCurveToTrinityTask(uploader: TrinityUploader, marketDataIdentifier: => MarketDataIdentifier) extends ScheduledTask {
  override def attributes = super.attributes + "DataSink" → "Trinity"

  def execute(observationDay: Day) = uploader.uploadCurve(CurveLabel(CurveTypeLabel("Price"), marketDataIdentifier,
    EnvironmentSpecificationLabel(observationDay, ClosesEnvironmentRule.label)))
}