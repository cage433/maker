package starling.gui.namedquantitycomponents

import starling.gui.GuiUtils._
import swing.Swing._
import swing.event.MouseClicked
import starling.quantity._
import starling.pivot.view.swing.{PivotCellRenderer, MigPanel}
import javax.swing.border.AbstractBorder
import swing.{Panel, Label}
import java.awt.{Color, Cursor, BasicStroke, Graphics, Insets, Graphics2D, Dimension}

class ExpandCollapsePanel(namedQuantity:NamedQuantity) extends MigPanel("insets 0") {
  private var expanded = false

  lazy val expandedPanel = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => {
        new BinOpNamedQuantityPanel(binOp)
      }
      case nq:NamedQuantity => {
        nq.quantity match {
          case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp)
          case nq0:NamedQuantity => new ExpandCollapsePanel(nq0)
          case q:Quantity => new QuantityPanel(q)
        }
      }
    }
  }
  lazy val lShapePanel = new LShape

  val label = new Label {
    text = namedQuantity.name
    tooltip = namedQuantity.toString
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
    font = PivotCellRenderer.MonoSpacedFont
    border = UnderLineDashedBorder()
  }

  add(label)

  reactions += {
    case MouseClicked(`label`,_,_,_,_) => {
      if (expanded) {
        remove(expandedPanel)
        remove(lShapePanel)
      } else {
        add(lShapePanel, "newline 0, split, spanx, gapright 0, ay top")
        add(expandedPanel, "gapleft 0")
      }
      expanded = !expanded
      revalidate()
      repaint()
    }
  }
  listenTo(label.mouse.clicks)
}

class QuantityPanel(quantity:Quantity) extends Label {
  text = quantity.toString
  font = PivotCellRenderer.MonoSpacedFont
}

class BinOpNamedQuantityPanel(binOp:BinOpNamedQuantity) extends MigPanel("insets 0", "[p]2lp[p][p][p]0[p]") {
  def panel(namedQuantity:NamedQuantity) = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp)
      case _ => new ExpandCollapsePanel(namedQuantity)
    }
  }
  add(new Label("(") {font = PivotCellRenderer.MonoSpacedFont}, "ay top")
  add(panel(binOp.lhs), "ay top")
  add(new Label(binOp.op) {font = PivotCellRenderer.MonoSpacedFont; tooltip = binOp.result.toString}, "ay top")
  add(panel(binOp.rhs), "ay top")
  add(new Label(")") {font = PivotCellRenderer.MonoSpacedFont}, "ay top")
}

class TopNamedQuantityComponent(quantity:SimpleNamedQuantity) extends MigPanel {
  quantity.quantity match {
    case nq:NamedQuantity => {
      add(new ExpandCollapsePanel(quantity))
      add(new Label("=") {font = PivotCellRenderer.MonoSpacedFont}, "ay top")
      add(new Label(nq.quantity.toString) {font = PivotCellRenderer.MonoSpacedFont}, "ay top")
    }
    case _ => {
      println("don't know")
    }
  }
}

object LShape {
  val colour = Color.GREEN.darker()
  val stroke = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1, Array(1.0f, 1.0f), 0)
}

import LShape._

class LShape extends Label {
  text = "   "
  override protected def paintComponent(g:Graphics2D) {
    g.setColor(colour)
    g.setStroke(stroke)

    val startWidth = 6
    val w = size.width - 1
    val h = (size.height / 2) + 1

    g.drawLine(startWidth, 1, startWidth, h)
    g.drawLine(startWidth, h, w, h)
  }
}

case class UnderLineDashedBorder() extends AbstractBorder {
  override def paintBorder(c:java.awt.Component, g:Graphics, x:Int, y:Int, width:Int, height:Int) {
    val g2 = g.asInstanceOf[Graphics2D]

    val s = c.getSize
    val w = s.width - 1
    val h = s.height - 1

    g2.setColor(colour)
    g2.setStroke(stroke)
    g2.drawLine(2,h,w,h)
  }

  override def getBorderInsets(c:java.awt.Component, insets:Insets) = {
    insets.left = 1
    insets.right = 1
    insets.top = 1
    insets.bottom = 1
    insets
  }
  override def getBorderInsets(c:java.awt.Component) = new Insets(1,1,1,1)
}

object NamedQuantityComponent {
  def main(args:Array[String]) {
    val price = Quantity(10.0, UOM.USD / UOM.BBL).named("F")
    val strike = Quantity(8.0, UOM.USD / UOM.BBL).named("K")
    val volume = Quantity(100.0, UOM.BBL).named("Volume")
    val priceTimesVolume = ((price - strike) * volume) * new Quantity(0.9).named("Discount")
    val topQuantity = SimpleNamedQuantity("P&L", priceTimesVolume)
    onEDT(showInFrame(new TopNamedQuantityComponent(topQuantity), pack0 = false))
  }
}