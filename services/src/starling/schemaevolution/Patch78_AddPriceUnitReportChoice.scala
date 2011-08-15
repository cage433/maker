package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.pivot.PivotFieldsState
import starling.utils.StarlingXStream
import starling.utils.sql.PersistAsBlob
import xstream.{ModifyClassWhichCanBeReadConverter, Fields, Reader, MapBasedConverter}
import starling.services.StarlingInit

class Patch78_AddPriceUnitReportChoice  extends Patch {

  def patchDescription = "AddPriceUnitReportChoice"

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(new ModifyClassWhichCanBeReadConverter() {
      def classToFix = classOf[PivotFieldsState]
      def fix(any: AnyRef) = {
        val pfs = any.asInstanceOf[PivotFieldsState]
        pfs.copy(reportSpecificChoices = (pfs.reportSpecificChoices + ("Price Unit"->"Position")))
      }
    })
    starling.inTransaction {
      writer => {
        writer.queryForUpdate("select layout from PivotLayouts") {
          rs => {
            val data = rs.getString("layout")
            val fixed = convertingXStream.fromXML(data)
            rs.update(Map("layout" -> PersistAsBlob(fixed)))
          }
        }
      }
    }
  }
}