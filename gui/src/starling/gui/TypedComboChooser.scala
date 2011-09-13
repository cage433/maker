package starling.gui

import swing._
import event._
import swing.ListView.Renderer
import starling.browser.common.MigPanel

abstract class TypedComboChooser[A](value0: A, values: List[A], typeRenderer: Renderer[A]) extends MigPanel("insets 0", "[p]") {
  val combo = new ComboBox(values) {
    renderer = typeRenderer
    selection.item = value0
  }

  add(combo, "push, grow")

  reactions += {
    case SelectionChanged(`combo`) => publish
  }

  listenTo(combo.selection)

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    combo.enabled = b
  }

  def value : A = combo.selection.item
  def value_=(value:A) = combo.selection.item = value

  protected def publish() : Unit
}
