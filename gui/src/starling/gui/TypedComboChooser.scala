package starling.gui

import swing._
import event._
import swing.ListView.Renderer
import starling.browser.common.MigPanel
import java.awt.event.{ItemEvent, ItemListener}

abstract class TypedComboChooser[A](value0: A, values: List[A], typeRenderer: Renderer[A]) extends MigPanel("insets 0", "[p]") {
  val combo = new ComboBox(values) {
    renderer = typeRenderer
    selection.item = value0
  }
  var previousSelection:Option[A] = None
  combo.peer.addItemListener(new ItemListener {
    def itemStateChanged(e:ItemEvent) {
      if (e.getStateChange == ItemEvent.SELECTED) {
        publish
      } else {
        previousSelection = values.find(_ == e.getItem)
      }
    }
  })

  add(combo, "push, grow")

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    combo.enabled = b
  }

  def value : A = combo.selection.item
  def value_=(value:A) = combo.selection.item = value

  protected def publish() : Unit
}
