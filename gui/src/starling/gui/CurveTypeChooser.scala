package starling.gui

import api.CurveTypeLabel
import pages.Revertable
import swing.ListView
import swing.event.Event

import utils.RichReactor._

case class CurveTypeChangedEvent(selected:CurveTypeLabel) extends Event

class CurveTypeChooser(pageContext:PageContext, selected:CurveTypeLabel)
  extends TypedComboChooser[CurveTypeLabel](selected, pageContext.localCache.curveTypes, ListView.Renderer(_.name))
  with Revertable {

  protected def publish() = publish(CurveTypeChangedEvent(value))

  def revert() = this.suppressingSelf(value = selected)
}