package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.utils.StarlingXStream
import xstream.{Fields, Reader, MapBasedConverter}
import starling.pivot.model.CollapsedState
import starling.pivot.{Field, Totals, OtherLayoutInfo}
import starling.pivot.HiddenType._
import starling.utils.sql.PersistAsBlob

class Patch112_UpdateBookmarkOtherLayoutInfo extends Patch {
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

        val hiddenType = if (fieldPanelCollapsed) FieldListHidden else NothingHidden

        OtherLayoutInfo(totals, frozen, rowCollapsedState, columnCollapsedState, newDisabledSubTotals, hiddenType = hiddenType)
      }
    },
    Map("fieldPanelCollapsed" -> classOf[Boolean])
  ))

  protected def runPatch(starlingInit:StarlingInit, starling:RichDB, writer:DBWriter) {
    val bookmarkSQL = "select * from Bookmarks"
    writer.queryForUpdate(bookmarkSQL) {
      rs => {
        val bookmark = rs.getString("bookmark")
        if (!bookmark.contains("oldFrozen") && !bookmark.contains("oldHiddenType")) {
          val newBookmark = convertingXStream.fromXML(bookmark)
          rs.update(Map("bookmark" -> new PersistAsBlob(newBookmark)))
        }
      }
    }
  }
}