package starling.pivot.view.swing.fieldchoosers

import starling.pivot.FieldChooserType.FieldChooserType
import starling.pivot.Field
import java.awt.Rectangle

trait DropTarget {
  // All the places you can drop a field, in the screen coordinate system.
  def dropBounds(draggedField:Field):List[Rectangle]
  def fieldChooserType:FieldChooserType
  def show(draggedField:Field)
  def hide()
  def reset()
  def extraFormatInfoUpdated()
}