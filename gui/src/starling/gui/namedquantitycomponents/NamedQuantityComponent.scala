package starling.gui.namedquantitycomponents

import starling.gui.GuiUtils._
import swing.Swing._
import swing.event.MouseClicked
import starling.quantity._
import starling.pivot.view.swing.{PivotCellRenderer, MigPanel}
import javax.swing.border.AbstractBorder
import swing.Label
import java.awt.{Color, Cursor, BasicStroke, Graphics, Insets, Graphics2D, Dimension}
import javax.swing.JTable
import javax.swing.table.{DefaultTableCellRenderer, AbstractTableModel}

object NamedQuantityComponentHelper {
  def panel(namedQuantity:NamedQuantity) = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round)
      case _ => new ExpandCollapsePanel(namedQuantity)
    }
  }

  def label(text0:String, tooltip0:String=null) = {
    new Label {
      font = PivotCellRenderer.MonoSpacedFont
      text = text0
      tooltip = tooltip0
    }
  }

  def row(namedQuantity:NamedQuantity):Option[List[String]] = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => (row(binOp.lhs), row(binOp.rhs)) match {
        case (Some(l), Some(r)) => Some(l ::: r)
        case _ => None
      }
      case SimpleNamedQuantity(name, q:NamedQuantity) => row(q).map(r => name :: r)
      case SimpleNamedQuantity(name, q:Quantity) => Some(name :: q.toString :: Nil)
      case _ => None
    }
  }
}
import NamedQuantityComponentHelper._

class ExpandCollapsePanel(namedQuantity:NamedQuantity) extends MigPanel("insets 0") {
  background = ExplanationPanelBackgroundColour
  private var expanded = false

  lazy val expandedPanel = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round)
      case nq:NamedQuantity => {
        nq.quantity match {
          case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp)
          case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func)
          case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func)
          case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate)
          case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert)
          case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round)
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

class FunctionNamedQuantityPanel(func:FunctionNamedQuantity) extends MigPanel("insets 0", "[p]0[p]0[p]0[p]", "[2lp]0[p]0[2lp]") {
  background = ExplanationPanelBackgroundColour
  add(new Brace(true), "skip 1, spany, growy")
  add(new Brace(false), "skip 3, spany, growy, wrap")
  add(label(func.functionName, func.result.toString), "gapright 2lp, ay top")
  val holderPanel = new MigPanel("insets 0") {
    background = ExplanationPanelBackgroundColour
    func.parameters.zipWithIndex.foreach{case (f,i) => {
      if (i != 0) {
        add(label(","), "ay top")
      }
      add(panel(f), "ay top")
    }}
  }
  add(holderPanel, "ay top, skip 2")
}

class VerticalFunctionNamedQuantityPanel(func:FunctionNamedQuantity) extends MigPanel("insets 0, gap 0") {
  background = ExplanationPanelBackgroundColour
  add(label(func.functionName, func.result.toString), "ay top")
  val table = func.parameters.map(row)
  if (!table.contains(None)) {
    val tableData = table.map(_.get)
    val tableModel = new AbstractTableModel {
      def getColumnCount = if (tableData.nonEmpty) tableData(0).size else 0
      def getRowCount = tableData.size
      def getValueAt(rowIndex:Int, columnIndex:Int) = tableData(rowIndex)(columnIndex)
    }
    val jTable = new JTable(tableModel) {
      setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)
      setBorder(MatteBorder(1, 1, 0, 0, TableGridColour))
      setCellSelectionEnabled(true)
    }
    val renderer = new DefaultTableCellRenderer {
      override def getTableCellRendererComponent(table:JTable, value:AnyRef, isSelected:Boolean, hasFocus:Boolean, row:Int, column:Int) = {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        setFont(PivotCellRenderer.MonoSpacedFont)
        this
      }
    }
    jTable.setDefaultRenderer(classOf[Object], renderer)
    resizeTableColumnsToFit(jTable, PivotCellRenderer.MonoSpacedFont)
    add(new LShape, "split, spanx, ay top, gapright 0, newline")
    add(jTable)
  } else {
    func.parameters.foreach(f => {
      add(new LShape, "split, spanx, ay top, gapright 0, newline")
      add(panel(f), "gapleft 0, ay top")
    })
  }
}

class Brace(left:Boolean) extends Label {
  minimumSize = new Dimension(5, 2)
  override protected def paintComponent(g:Graphics2D) {
    g.setColor(Color.BLACK)
    val w = size.width - 1
    val sh = 1
    val h = size.height - 2

    if (!left) {
      g.rotate(math.Pi, w / 2.0, (size.height-1) / 2.0)
    }

    g.drawLine(0, sh, w, sh)
    g.drawLine(0, sh, 0, h)
    g.drawLine(0, h, w, h)
  }
}

class BinOpNamedQuantityPanel(binOp:BinOpNamedQuantity) extends MigPanel("insets 0", "[p]0[p][p][p]0[p]", "[2lp]0[p]0[2lp]") {
  background = ExplanationPanelBackgroundColour
  add(new Brace(true), "growy, spany")
  add(new Brace(false), "skip 3, growy, spany, wrap")
  add(panel(binOp.lhs), "ay top, skip 1")
  add(label(binOp.op, binOp.result.toString), "ay top")
  add(panel(binOp.rhs), "ay top")
}

class NegateNamedQuantityPanel(negate:NegateNamedQuantity) extends MigPanel("insets 0", "[p]0[p]") {
  background = ExplanationPanelBackgroundColour
  add(label("-", negate.quantity.toString), "ay top")
  add(panel(negate.qty))
}

class InvertNamedQuantityPanel(invert:InvertNamedQuantity) extends MigPanel("insets 0", "[p]0[p]0[p]0[p]", "[2lp]0[p]0[2lp]") {
  background = ExplanationPanelBackgroundColour
  add(new Brace(true), "growy, spany")
  add(new Brace(false), "skip 3, growy, spany, wrap")
  add(label("1/", invert.quantity.toString), "ay top, skip 1")
  add(panel(invert.qty), "ay top")
}

class RoundedNamedQuantityPanel(round:RoundedNamedQuantity) extends MigPanel("insets 0", "[p]0", "[2lp]0[p]0[2lp]") {
  background = ExplanationPanelBackgroundColour
  add(new Brace(true), "skip 1, spany, growy")
  add(new Brace(false), "skip 4, spany, growy, wrap")
  add(label("Round", round.quantity.toString), "gapright 2lp, ay top")
  add(panel(round.orig), "skip 1, ay top")
  add(label(", " + round.dp.toString), "ay top")
}

class TopNamedQuantityComponent(quantity:SimpleNamedQuantity) extends MigPanel {
  background = ExplanationPanelBackgroundColour
  quantity.quantity match {
    case nq:NamedQuantity => {
      add(new ExpandCollapsePanel(quantity))
      add(label("="), "ay top")
      add(label(nq.quantity.toString), "ay top")
    }
    case _ => throw new Exception("Haven't thought about what to do here")
  }
}

object LShape {
  val colour = Color.GREEN.darker()
  val stroke = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1, Array(1.0f, 1.0f), 0)

  val MaxHeight = {
    val l = label("W")
    l.preferredSize.height
  }
}

import LShape._

class LShape extends Label {
  minimumSize = new Dimension(12,MaxHeight)
  maximumSize = new Dimension(12,MaxHeight)
  override protected def paintComponent(g:Graphics2D) {
    g.setColor(colour)
    g.setStroke(stroke)

    val startWidth = 6
    val w = size.width - 1
    val h = (size.height / 2)

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
    val discount = new Quantity(0.9).named("Discount")
    val priceTimesVolume = ((price - strike) * volume) * discount

    val func = FunctionNamedQuantity("Sum", List(price.negate, strike.invert), price + strike, true) * discount.round(3)

//    val topQuantity = SimpleNamedQuantity("P&L", priceTimesVolume)
    val topQuantity = SimpleNamedQuantity("P&L", func)

    onEDT(showInFrame(new TopNamedQuantityComponent(topQuantity), pack0 = false))
  }
}