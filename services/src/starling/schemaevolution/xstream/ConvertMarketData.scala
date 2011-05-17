package starling.schemaevolution.xstream

import com.thoughtworks.xstream.converters.Converter
import starling.richdb.{RichDB}
import starling.utils.StarlingXStream
import com.thoughtworks.xstream.XStream
import starling.utils.sql.Clause
import starling.utils.sql.QueryBuilder._
import starling.db.DBWriter

class ConvertMarketData(dataTypeKey : String, convertingXStream : XStream){

  val standardXStream = StarlingXStream.createXStream

  def apply(starlingDB : RichDB, writer : DBWriter){
    starlingDB.query(
      """
        select snapshotID, subTypeKey, data
          from SnapshotData
        where
          dataTypeKey = :dataTypeKey
      """,
      Map("dataTypeKey" -> dataTypeKey)
    ) {
      rs => {
        val snapshotID = rs.getInt("snapshotID")
        val subTypeKey = rs.getString("subTypeKey")
        val xml = rs.getString("data")
        val convertedObject = convertingXStream.fromXML(xml)
        val newXML = standardXStream.toXML(convertedObject)

        println("Converting " + snapshotID + ", " + subTypeKey)
        rs.updateString("data", newXML)
      }
    }

  }

}