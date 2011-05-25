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
import starling.utils.sql.AnObject

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

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val sql = "select layout from PivotLayouts"
    writer.queryForUpdate(sql) {
      rs => {
        val layout = rs.getString("layout")
        val newLayout = convertingXStream.fromXML(layout)
        rs.update(Map("layout" -> new AnObject(newLayout)))
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