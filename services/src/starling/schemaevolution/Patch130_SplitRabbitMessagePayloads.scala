package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.daterange.Timestamp
import starling.db.{ResultSetRow, DBWriter}


class Patch130_SplitRabbitMessagePayloads extends Patch{

  var nextStarlingID : Long = 0
  def makeNewRows (rsr : ResultSetRow) : List[Map[String, Any]] = {

    val verb = rsr.getString("verb")
    val subject = rsr.getString("subject")
    val id = rsr.getString("id")
    val source = rsr.getString("source")
    val timestamp = rsr.getTimestamp("timestamp")
    val host = rsr.getString("host")
    val pid = rsr.getInt("pid")
    val body = rsr.getString("body")
    val payloads: List[String] = rsr.getString("payloads").split(",").toList
    val starlingTimestamp = rsr.getTimestamp("starlingTimestamp")
    payloads.map{
      payload  =>
        var (pType, pValue) = try {
          payload.splitAt(payload.indexOf(':'))
        } catch {
          case e =>
            println("payloads " + payload)
            throw e
        }
        pType = pType.trim
        pValue = pValue.drop(1).trim
        val result = Map(
          "starlingID" -> nextStarlingID,
          "verb" -> verb, 
          "subject" -> subject, 
          "id" -> id, 
          "source" -> source, 
          "timestamp" -> timestamp, 
          "host" -> host, 
          "pid" -> pid, 
          "body" -> body, 
          "payloadType" -> pType,
          "payloadValue" -> pValue,
          "starlingTimestamp" -> starlingTimestamp
        )
      nextStarlingID = nextStarlingID + 1
      result
    }
  }

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    var newRows = List[Map[String, Any]]()
    starling.query("select * from RabbitMessages"){
      rsr =>
        newRows = makeNewRows(rsr).reverse ::: newRows
    }
    writer.update("truncate table RabbitMessages")
    writer.update("alter table RabbitMessages drop column payloads")
    writer.update("alter table RabbitMessages add payloadType [varchar](max) NULL")
    writer.update("alter table RabbitMessages add payloadValue [varchar](max) NULL")

    newRows.reverse.grouped(1000).foreach{
      rows =>
        writer.insert("RabbitMessages", rows)
    }

  }

}
