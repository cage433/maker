package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import collection.immutable.TreeMap
import collection.SortedMap
import scala.Some
import starling.pivot._
import model.CollapsedState
import starling.utils.sql.PersistAsBlob
import starling.pivot.HiddenType._

class Patch97_ConvertColumnStructure extends Patch {


  val convertingXStream = StarlingXStream.createXStream
  convertingXStream.registerConverter(new MapBasedConverter(
    StarlingXStream.createXStream,
    classOf[PivotFieldsState],
    new Reader {
      def create(fields:Fields) = {
        val rowFields = fields.getFieldValue("rowFields").getOrElse(List[Field]()).asInstanceOf[List[Field]]
        val columns = fields.getFieldValue("columns").getOrElse(ColumnStructure(Field("ROOT"), false, Nil)).asInstanceOf[ColumnStructure]
        val filters = fields.getFieldValue("filters").getOrElse(List[Field]()).asInstanceOf[List[(Field,Selection)]]
        val treeDepths:SortedMap[Field,(Int,Int)] = fields.getFieldValue("treeDepths") match {
          case Some(td) => td.asInstanceOf[SortedMap[Field,(Int,Int)]]
          case None => TreeMap.empty
        }
        val reportSpecificChoices:SortedMap[String,Any] = fields.getFieldValue("reportSpecificChoices") match {
          case Some(rsc) => rsc.asInstanceOf[SortedMap[String,Any]]
          case None => TreeMap.empty
        }
        val transforms:SortedMap[Field,FilterWithOtherTransform] = fields.getFieldValue("transforms") match {
          case Some(t) => t.asInstanceOf[SortedMap[Field,FilterWithOtherTransform]]
          case None => TreeMap.empty
        }

        val newColumns = ColumnTrees(columns.children.map(_.toColumnTree))

        new PivotFieldsState(rowFields, newColumns, filters, treeDepths, reportSpecificChoices, transforms)
      }
    },
    Map("columns" -> classOf[ColumnStructure])
  ))

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

        val hiddenType = if (fieldPanelCollapsed) FieldListHidden else NothingHidden

        OtherLayoutInfo(totals, frozen, rowCollapsedState, columnCollapsedState, newDisabledSubTotals, hiddenType = hiddenType)
      }
    },
    Map("rowSubTotalsDisabled" -> classOf[List[Field]], "columnSubTotalsDisabled" -> classOf[List[Field]], "fieldPanelCollapsed" -> classOf[Boolean])
  ))

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select layout from PivotLayouts"
    writer.queryForUpdate(sql) {
      rs => {
        val layout = rs.getString("layout")
        val newLayout = convertingXStream.fromXML(layout)
        rs.update(Map("layout" -> new PersistAsBlob(newLayout)))
      }
    }

    val userSettingsSQL = "select settings from usersettings"
    writer.queryForUpdate(userSettingsSQL) {
      rs => {
        val settings = rs.getString("settings")
        val newSettings = convertingXStream.fromXML(settings)
        rs.update(Map("settings" -> new PersistAsBlob(newSettings)))
      }
    }

    val sql2 = "select otherLayoutInfo from PivotLayouts"
    writer.queryForUpdate(sql2) {
      rs => {
        val otherInfo = rs.getString("otherLayoutInfo")
        val newOtherInfo = convertingXStream.fromXML(otherInfo)
        rs.update(Map("otherLayoutInfo" -> new PersistAsBlob(newOtherInfo)))
      }
    }
  }
}