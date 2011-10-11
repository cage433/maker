package starling.rmi

import starling.db.DB
import com.trafigura.shared.events.Event
import starling.dbx.QueryBuilder._
import starling.daterange.Timestamp
import starling.gui.api.RabbitEventReceived
import starling.utils.Broadcaster
import starling.tradestore.TradeStore.StoreResults

object DefaultRabbitEventDatabase {
  val TableName = "RabbitMessages"
}

trait EventStatus { def code : String }
case object UnprocessedEventStatus extends EventStatus { def code = "U" }
case object ProcessedEventStatus extends EventStatus { def code = "P" }
case object ErroredEventStatus extends EventStatus { def code = "E" }

import DefaultRabbitEventDatabase._

trait RabbitEventDatabase {
  def saveEvent(e : Event, status : EventStatus = UnprocessedEventStatus, result : Option[String] = None)
  def updateEvent(e : Event, status : EventStatus, ex : Option[Throwable] = None, result : Option[String] = None, tradeStoreResults : Option[StoreResults] = None)
  def latestID : Long
  def db : DB
}

object NullRabbitEventDatabase extends RabbitEventDatabase {
  def saveEvent(e : Event, status : EventStatus = UnprocessedEventStatus, result : Option[String] = None) = {}
  def updateEvent(e : Event, status : EventStatus, ex : Option[Throwable] = None, result : Option[String] = None, tradeStoreResults : Option[StoreResults]) = {}
  def latestID : Long = 0
  def db : DB = null
}

class DefaultRabbitEventDatabase(val db : DB, broadcaster : Broadcaster) extends RabbitEventDatabase {
  private val colSize = 300
  private var maxID = {
    val q = (
            select("max(starlingID) maxID")
                    from (TableName)
            )
    db.queryWithOneResult(q) {row => {if (row.isNull("maxID")) -1L else row.getLong("maxID")}} match {
      case None => -1L
      case Some(l) => l
    }
  }

  def latestID = synchronized {maxID}

  def saveEvent(e : Event, status : EventStatus, result : Option[String] = None) {
    synchronized {
      val verb:String = e.verb.toJson.take(colSize)
      val subject:String = e.subject.take(colSize)
      val id:String = e.key.identifier.take(colSize)
      val source:String = e.source.take(colSize)
      val timestamp:Timestamp = Timestamp(e.content.header.timestamp.getMillis)
      val host:String = e.content.header.host.take(colSize)
      val pid:Int = e.content.header.pid
      val body = e.content.body.toJson.toString
      val starlingTimestamp = new Timestamp

      db.inTransaction{
        writer => {
          e.content.body.payloads.foreach {
            p =>
              maxID += 1
              writer.insert(
                TableName,
                Map(
                  "starlingID" -> maxID,
                  "verb" -> verb,
                  "subject" -> subject,
                  "id" -> id,
                  "source" -> source,
                  "timestamp" -> timestamp,
                  "host" -> host,
                  "pid" -> pid,
                  "body" -> body,
                  "starlingtimestamp" -> starlingTimestamp,
                  "payloadType" -> p.payloadType.trim,
                  "payloadValue" -> p.key.identifier.trim,
                  "status" -> status.code
                ) ++ result.map("result" -> _)
                  ++ (status match { case ProcessedEventStatus => Some(""); case _ => None }).map("errorMsg" -> _)
              )
          }
        }
      }

      broadcaster.broadcast(RabbitEventReceived(maxID))
    }
  }

  def updateEvent(e : Event,
                  status : EventStatus,
                  ex : Option[Throwable] = None,
                  result : Option[String] = None,
                  tradeStoreResults : Option[StoreResults]) = {
    synchronized {
      val verb:String = e.verb.toJson.take(colSize)
      val subject:String = e.subject.take(colSize)
      val id:String = e.key.identifier.take(colSize)
      val source:String = e.source.take(colSize)
      val timestamp:Timestamp = Timestamp(e.content.header.timestamp.getMillis)
      val host:String = e.content.header.host.take(colSize)
      val pid:Int = e.content.header.pid
      val body = e.content.body.toJson.toString
      val starlingTimestamp = new Timestamp

      val tradeStoreUpdate = tradeStoreResults.map(tsr =>
                                Map("tradeInsertedCount" -> tsr.inserted,
                                    "tradeUpdatedCount" -> tsr.updated,
                                    "tradeDeletedCount" -> tsr.deleted))

      db.inTransaction {
        writer => {
          e.content.body.payloads.foreach{
            p =>
              writer.update(
                TableName,
                Map(
                  "verb" -> verb,
                  "subject" -> subject,
                  "id" -> id,
                  "source" -> source,
                  "timestamp" -> timestamp,
                  "host" -> host,
                  "pid" -> pid,
                  "body" -> body,
                  "starlingtimestamp" -> starlingTimestamp,
                  "payloadType" -> p.payloadType.trim,
                  "payloadValue" -> p.key.identifier.trim,
                  "status" -> status.code
                ) ++ result.map("result" -> _)
                  ++ ex.map("errorMsg" -> _.getMessage) ++ tradeStoreUpdate.flatten,
                ("id" eql id))
          }
        }
      }

      broadcaster.broadcast(RabbitEventReceived(maxID))
    }
  }
}
