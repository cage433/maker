package starling.services

import starling.pivot._


class SchedulerReferenceData(scheduler: Scheduler) extends UnfilteredPivotTableDataSource with ScheduledTaskAttributes {
  val group@List(name, timing, period, calendar, producer, consumer, sender, recipients, active) =
    fieldDetails("Task", "Starting Time", "Period", "Calendar", "Producer", "Consumer", "Sender", "Receipients", "Active")

  val fieldDetailsGroups = List(FieldDetailsGroup("Schedule", group))
  override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(group.tail))

  def unfilteredData(pfs: PivotFieldsState) = scheduler.getTasks.map { task => fields(
    name       → task.name,
    timing     → task.time.prettyTime,
    period     → task.time.description,
    calendar   → task.cal.name,
    producer   → task.attribute(DataSource),
    active     → task.isRunning,
    consumer   → task.attribute(DataSink),
    sender     → task.attribute(EmailFrom),
    recipients → task.attribute(EmailTo))
  }
}