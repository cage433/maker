package starling.rmi

import starling.db.DB
import com.trafigura.shared.events.Event
import starling.dbx.QueryBuilder._
import starling.daterange.Timestamp
import starling.gui.api.RabbitEventReceived
import starling.utils.Broadcaster

object DefaultRabbitEventDatabase {
  val TableName = "RabbitMessages"
}
import DefaultRabbitEventDatabase._

/**
 * The RabbitEventDatabase provides a contract for accessing the Rabbit MQ database.  Along with an accessor to the
 * DB reference, it allows Events to be saved to the database and returns the ID of the latest event.
 *
 * @documented
 */
trait RabbitEventDatabase {
  def saveEvent(e : Event)
  def latestID : Long
  def db : DB
}

/**
 * NullRabbitEventDatabase provides a singleton implementation referencing a null DB.
 */
object NullRabbitEventDatabase extends RabbitEventDatabase {
  /**Does nothing.*/
  def saveEvent(e: Event) = {}
  /**@return 0*/
  def latestID : Long = 0
  /**@return null*/
  def db : DB = null
}

/**
 * DefaultRabbitEventDatabase provides a simple, default implementation of the RabbitEventDatabase.
 *
 * @documented
 */
class DefaultRabbitEventDatabase(val db:DB, broadcaster:Broadcaster) extends RabbitEventDatabase {
  private val colSize = 300
  // load the initial value
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

  // synchronizes on this instance
  def latestID = synchronized {maxID}

  // saves the event, sychronizing on this instance to write a new maximum ID
  def saveEvent(e:Event) {synchronized {
    maxID += 1
    val verb:String = e.verb.toJson.take(colSize)
    val subject:String = e.subject.take(colSize)
    val id:String = e.key.identifier.take(colSize)
    val source:String = e.source.take(colSize)
    val timestamp:Timestamp = Timestamp(e.content.header.timestamp.getMillis)
    val host:String = e.content.header.host.take(colSize)
    val pid:Int = e.content.header.pid
    val body = e.content.body.toJson.toString

    val payloads = e.content.body.payloads.map{p => p.payloadType +": " + p.key.identifier}.mkString(", ")

    db.inTransaction{
      writer => {
        writer.insert(TableName, Map(
          "starlingID" -> maxID,
          "verb" -> verb,
          "subject" -> subject,
          "id" -> id,
          "source" -> source,
          "timestamp" -> timestamp,
          "host" -> host,
          "pid" -> pid,
          "body" -> body,
          "starlingtimestamp" -> new Timestamp(),
          "payloads" -> payloads
        ))
      }
    }

    broadcaster.broadcast(RabbitEventReceived(maxID))
  }}
}
