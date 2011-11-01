package starling.services

import starling.pivot._
import starling.scheduler.{Scheduler, ScheduledTaskAttributes}


class SchedulerReferenceData(scheduler: Scheduler) extends UnfilteredPivotTableDataSource with ScheduledTaskAttributes {
  val group@List(name, timing, period, calendar, producer, consumer, sender, recipients, enabled) =
    fieldDetails("Task", "Starting Time", "Period", "Calendar", "Producer", "Consumer", "Sender", "Receipients", "Enabled")

  val fieldDetailsGroups = List(FieldDetailsGroup("Schedule", group))
  override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(group.tail))

  def unfilteredData(pfs: PivotFieldsState) = scheduler.getTasks.map { task => fields(
    name       → task.name,
    timing     → task.time.prettyTime,
    period     → task.time.description,
    calendar   → task.cal.name,
    producer   → task.attribute(DataSource),
    enabled    → task.isEnabled,
    consumer   → task.attribute(DataSink),
    sender     → task.attribute(EmailFrom),
    recipients → task.attribute(EmailTo))
  }
}