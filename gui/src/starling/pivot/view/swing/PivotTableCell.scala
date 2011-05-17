package starling.pivot.view.swing

import java.awt.{Font, Color}
import scala.swing.{Alignment, Label}

class PivotTableCell(text: String, isHighlighted: Boolean,locationType: CellLocationType.Value) {
  private val FontName = "Arial"
  private val DropText = "<Drop Field Here>"
  private val FontSize = 12

  def canDrop = locationType!=CellLocationType.DataValue // can drop on anything but a data value cell
  def location = locationType
  def this() = this("", false, CellLocationType.DataValue)

  private def onAxis = (locationType==CellLocationType.ColAxis || locationType==CellLocationType.RowAxis)
  private def onDataField = (locationType==CellLocationType.DataField)

  def label : Label = {
     val isOnAxis = onAxis
     val txt = if(text == null) "" else text 
     val fontName = FontName
     new Label(txt) {
       val style = if (isOnAxis) (Font.BOLD | Font.ITALIC) else 0
       val fgColor = if (isHighlighted) Color.BLUE else Color.BLACK
       val bgColor = if (onDataField) Color.CYAN
                     else if(isOnAxis) Color.LIGHT_GRAY
                     else Color.WHITE 
       font = new Font(FontName, style, FontSize)
       peer.setForeground(fgColor)
       xAlignment = Alignment.Left
       peer.setOpaque(true)
       peer.setBackground(bgColor)
     }
  }

  def isEmpty = text.isEmpty

  override def toString = "text="+text+" highlighted="+isHighlighted+" location="+locationType
}


