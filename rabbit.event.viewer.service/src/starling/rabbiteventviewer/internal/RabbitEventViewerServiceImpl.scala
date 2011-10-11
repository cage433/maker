package starling.rabbiteventviewer.internal

import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.pivot.model.PivotTableModel
import starling.pivot._
import starling.rmi.{DefaultRabbitEventDatabase, RabbitEventDatabase}
import starling.dbx.{Clause, From, RealTable}
import starling.daterange.{Timestamp, Day}
import utils.{TimestampPivotFormatter, TimeOnlyTimestampPivotFormatter}

class RabbitEventViewerServiceImpl(eventDatabase:RabbitEventDatabase) extends RabbitEventViewerService {
  def rabbitEvents(pivotFieldParams:PivotFieldParams, latestEvent:Long) = {
    val table = DefaultRabbitEventDatabase.TableName
    val starlingID = "Starling ID"
    val verb = "Verb"
    val subject = "Subject"
    val id = "ID"
    val source = "Source"
    val timestamp = "Message Time (UTC)"
    val starlingTimestamp = "Received Time (UK)"
    val host = "Host"
    val pid = "PID"
    val body = "Body"
    val payloadType = "Payload Type"
    val payloadValue = "Payload Value"

    val status = "Status"
    val errorMsg = "Error Message"
    val result = "Result"
    val tradeInsertedCount = "Trade inserted count"
    val tradeUpdatedCount = "Trade update count"
    val tradeDeletedCount = "Trade deleted count"

    val columns = {
      List(("Event Fields", List(
        new LongColumnDefinition(starlingID, "starlingID", table) {
          override def fieldDetails:FieldDetails = new FieldDetails(name) {
            override def comparator = new Ordering[Any]() {
              def compare(x: Any, y: Any) = y.asInstanceOf[Long].compare(x.asInstanceOf[Long])
            }
          }
        },
        StringColumnDefinition(verb, "verb", table),
        StringColumnDefinition(subject, "subject", table),
        StringColumnDefinition(id, "id", table),
        StringColumnDefinition(source, "source", table),
        new TimestampColumnDefinition(timestamp, "timestamp", table) {
          override def fieldDetails:FieldDetails = new FieldDetails(name) {
            override def formatter = TimeOnlyTimestampPivotFormatter
            override def comparator = new Ordering[Any]() {
              def compare(x: Any, y: Any) = y.asInstanceOf[Timestamp].compare(x.asInstanceOf[Timestamp])
            }
          }
        },
        new TimestampColumnDefinition(starlingTimestamp, "starlingTimestamp", table) {
          override def fieldDetails:FieldDetails = new FieldDetails(name) {
            override def formatter = TimestampPivotFormatter
            override def comparator = new Ordering[Any]() {
              def compare(x: Any, y: Any) = y.asInstanceOf[Timestamp].compare(x.asInstanceOf[Timestamp])
            }
          }
        },
        new DayColumnDefinition("Day", table) {
          override val fullSqlName = "timestamp"
          override def filterClauses(values:Set[Any]):List[Clause] = values.toList.map { value => {
            val day = value.asInstanceOf[NullableDay].day
            (starling.dbx.Field("timestamp") gt day.toSqlDate) and (starling.dbx.Field("timestamp") lte day.nextDay.toSqlDate)
          } }
          override def fieldDetails:FieldDetails = new FieldDetails(name) {
            override def comparator = new Ordering[Any]() {
              def compare(x: Any, y: Any) = y.asInstanceOf[NullableDay].day.compare(x.asInstanceOf[NullableDay].day)
            }
          }
        },
        StringColumnDefinition(host, "host", table),
        new IntColumnDefinition(pid, "pid", table),
        StringColumnDefinition(body, "body", table),
        StringColumnDefinition(payloadType, "payloadType", table),
        StringColumnDefinition(payloadValue, "payloadValue", table),
        StringColumnDefinition(status, "status", table),
        StringColumnDefinition(errorMsg, "errorMsg", table),
        StringColumnDefinition(result, "result", table),
        StringColumnDefinition(tradeInsertedCount, "tradeInsertedCount", table),
        StringColumnDefinition(tradeUpdatedCount, "tradeUpdatedCount", table),
        StringColumnDefinition(tradeDeletedCount, "tradeDeletedCount", table)
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