package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import starling.utils.sql.AnObject
import starling.pivot._
import model.CollapsedState

class Patch98_ConvertOtherLayoutInfo extends Patch {

  val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[OtherLayoutInfo],
    new Reader {
      def create(fields:Fields) = {
        val totals = fields.getFieldValue("totals").getOrElse(Totals.Null).asInstanceOf[Totals]
        val frozen = fields.getFieldValue("frozen").getOrElse(true).asInstanceOf[Boolean]
        val fieldPanelCollapsed = fields.getFieldValue("fieldPanelCollapsed").getOrElse(false).asInstanceOf[Boolean]
        val rowCollapsedState = fields.getFieldValue("rowCollapsedState").getOrElse(CollapsedState.None).asInstanceOf[CollapsedState]
        val columnCollapsedState = fields.getFieldValue("columnCollapsedState").getOrElse(CollapsedState.None).asInstanceOf[CollapsedState]

        val rowSubTotalsDisabled = fields.getFieldValue("rowSubTotalsDisabled").getOrElse(List()).asInstanceOf[List[Field]]
        val columnSubTotalsDisabled = fields.getFieldValue("columnSubTotalsDisabled").getOrElse(List()).asInstanceOf[List[Field]]
        val newDisabledSubTotals = (rowSubTotalsDisabled ::: columnSubTotalsDisabled).toSet.toList

        OtherLayoutInfo(totals, frozen, fieldPanelCollapsed, rowCollapsedState, columnCollapsedState, newDisabledSubTotals)
      }
    },
    Map("rowSubTotalsDisabled" -> classOf[List[Field]], "columnSubTotalsDisabled" -> classOf[List[Field]])
  ))

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select otherLayoutInfo from PivotLayouts"
    writer.queryForUpdate(sql) {
      rs => {
        val otherInfo = rs.getString("otherLayoutInfo")
        val newOtherInfo = convertingXStream.fromXML(otherInfo)
        rs.update(Map("otherLayoutInfo" -> new AnObject(newOtherInfo)))
      }
    }

    val userSettingsSQL = "select settings from usersettings"
    writer.queryForUpdate(userSettingsSQL) {
      rs => {
        val settings = rs.getString("settings")
        val newSettings = convertingXStream.fromXML(settings)
        rs.update(Map("settings" -> new AnObject(newSettings)))
      }
    }
  }
}