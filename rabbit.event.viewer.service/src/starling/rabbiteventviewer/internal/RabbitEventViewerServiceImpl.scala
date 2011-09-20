package starling.rabbiteventviewer.internal

import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.dbx.{From, RealTable}
import starling.pivot.model.PivotTableModel
import starling.pivot._
import starling.rmi.{DefaultRabbitEventDatabase, RabbitEventDatabase}

class RabbitEventViewerServiceImpl(eventDatabase:RabbitEventDatabase) extends RabbitEventViewerService {
  def rabbitEvents(pivotFieldParams:PivotFieldParams, latestEvent:Long) = {
    val table = DefaultRabbitEventDatabase.TableName
    val starlingID = "Starling ID"
    val verb = "Verb"
    val subject = "Subject"
    val id = "ID"
    val source = "Source"
    val timestamp = "Timestamp"
    val host = "Host"
    val pid = "PID"
    val body = "Body"

    val columns = {
      List(("Event Fields", List(
        new LongColumnDefinition(starlingID, "starlingID", table),
        StringColumnDefinition(verb, "verb", table),
        StringColumnDefinition(subject, "subject", table),
        StringColumnDefinition(id, "id", table),
        StringColumnDefinition(source, "source", table),
        new TimestampColumnDefinition(timestamp, "timestamp", table),
        new DayColumnDefinition("Day", table) {
          override val fullSqlName = "timestamp"
        },
        StringColumnDefinition(host, "host", table),
        new IntColumnDefinition(pid, "pid", table),
        StringColumnDefinition(body, "body", table)
      )))
    }

    PivotTableModel.createPivotData(new OnTheFlySQLPivotTableDataSource(
      eventDatabase.db,
      columns,
      From(RealTable(table), List()),
      List(),
      PivotFieldsState(),
      List()
    ), pivotFieldParams)
  }

  def latestRabbitEvent = eventDatabase.latestID
}