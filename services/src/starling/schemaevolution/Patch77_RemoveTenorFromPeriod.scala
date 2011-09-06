package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.instrument.utils.StarlingXStream
import starling.utils.sql.PersistAsBlob
import starling.curves.{SpreadStdDevSurfaceDataType}
import xstream.{Fields, Reader, MapBasedConverter}
import starling.daterange.{TenorType, Spread}
import starling.services.StarlingInit

class Patch77_RemoveTenorFromPeriod extends Patch {

  def patchDescription = "RemoveTenorFromPeriod"

  lazy val marketDataType = SpreadStdDevSurfaceDataType

  val mapBasedConverter = new MapBasedConverter(
      StarlingXStream.createXStream,
      classOf[Spread[_]],
      new Reader(){
        def create(fields: Fields) = {
          (fields.getFieldValue("tenor"): @unchecked) match {
            case Some(t) => {
              val tt = t.asInstanceOf[TenorType]
              val first = fields.getFieldValue("first").get.asInstanceOf[tt.T]
              val last = fields.getFieldValue("last").get.asInstanceOf[tt.T]
              Spread[tt.T](first, last)
            }
          }
        }
      }
    )

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(mapBasedConverter)
    starling.inTransaction {
      writer => {
        writer.queryForUpdate("select data from MarketData where marketdatatype = '" + StarlingXStream.write(marketDataType) + "'") {
          rs => {
            val data = rs.getString("data")
            println(data)
            val fixed = convertingXStream.fromXML(data)
            rs.update(Map("data" -> PersistAsBlob(fixed)))
          }
        }
      }
    }
  }
}