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
import starling.pivot.{QuantityLabelPivotFormatter, PivotFormatter, ExtraFormatInfo}
import collection.mutable.ListBuffer

object NamedQuantityComponentHelper {
  def panel(namedQuantity:NamedQuantity, fi:ExtraFormatInfo) = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi)
      case _ => new ExpandCollapsePanel(namedQuantity, fi)
    }
  }

  def label(text0:String, tooltip0:String=null) = {
    new Label {
      font = PivotCellRenderer.MonoSpacedFont
      text = text0
      tooltip = tooltip0
    }
  }

  def row(namedQuantity:NamedQuantity, fi:ExtraFormatInfo):Option[List[String]] = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => (row(binOp.lhs, fi), row(binOp.rhs, fi)) match {
        case (Some(l), Some(r)) => Some(l ::: r)
        case _ => None
      }
      case SimpleNamedQuantity(name, q:NamedQuantity) => row(q, fi).map(r => name :: r)
      case SimpleNamedQuantity(name, q:Quantity) => Some(name :: quantityText(q, fi) :: Nil)
      case _ => None
    }
  }

  def quantityText(q:Quantity, fi:ExtraFormatInfo) = QuantityLabelPivotFormatter.format(q, fi).text
}
import NamedQuantityComponentHelper._

class ExpandCollapsePanel(namedQuantity:NamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  private var expanded = false

  lazy val expandedPanel = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi)
      case nq:NamedQuantity => {
        nq.quantity match {
          case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi)
          case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi)
          case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi)
          case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi)
          case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi)
          case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi)
          case nq0:NamedQuantity => new ExpandCollapsePanel(nq0, fi)
          case q:Quantity => new QuantityPanel(q, fi)
        }
      }
    }
  }
  lazy val lShapePanel = new LShape

  val label = new Label {
    text = namedQuantity.name
    tooltip = quantityText(namedQuantity, fi)
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
    font = PivotCellRenderer.MonoSpacedFont
    border = UnderLineDashedBorder()
  }

  add(label)

  reactions += {
    case MouseClicked(`label`,_,_,_,_) => {expandCollapse()}
  }
  listenTo(label.mouse.clicks)

  def expandCollapse() {
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
    
  def updateExtraInfo(newFI:ExtraFormatInfo) {
    label.tooltip = quantityText(namedQuantity, newFI)
    expandedPanel.updateExtraInfo(newFI)
  }

  expandCollapse()
}

class QuantityPanel(quantity:Quantity, fi:ExtraFormatInfo) extends Label with UpdateableNamedQuantityComponent {
  text = quantityText(quantity, fi)
  font = PivotCellRenderer.MonoSpacedFont

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    text = quantityText(quantity, newFI)
  }
}

class FunctionNamedQuantityPanel(func:FunctionNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0", "[p]0[p]0[p]0[p]", "[2lp]0[p]0[2lp]") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val funcLabel = label(func.functionName, quantityText(func.result, fi))
  val allPanels = new ListBuffer[UpdateableNamedQuantityComponent]()

  add(new Brace(true), "skip 1, spany, growy")
  add(new Brace(false), "skip 3, spany, growy, wrap")
  add(funcLabel, "gapright 2lp, ay top")
  val holderPanel = new MigPanel("insets 0") {
    background = ExplanationPanelBackgroundColour
    func.parameters.zipWithIndex.foreach{case (f,i) => {
      if (i != 0) {
        add(label(","), "ay top")
      }
      val p = panel(f, fi)
      allPanels += p
      add(p, "ay top")
    }}
  }
  add(holderPanel, "ay top, skip 2")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    funcLabel.tooltip = quantityText(func.result, newFI)
    allPanels.foreach(_.updateExtraInfo(newFI))
  }
}

class VerticalFunctionNamedQuantityPanel(func:FunctionNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0, gap 0") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val funcLabel = label(func.functionName, quantityText(func.result, fi))
  val allPanels = new ListBuffer[UpdateableNamedQuantityComponent]()

  add(funcLabel, "ay top")

  val table = func.parameters.map(row(_, fi))
  if (!table.contains(None)) {
    def generateTableModel(table0:List[Option[scala.List[String]]]) = {
      val tableData0 = table0.map(_.get)
      val maxWidth = tableData0.map(_.length).max
      val tableData = tableData0.map(c => {
        List.fill(maxWidth - c.length)("") ::: c
      })
      new AbstractTableModel {
        def getColumnCount = if (tableData.nonEmpty) tableData(0).size else 0
        def getRowCount = tableData.size
        def getValueAt(rowIndex:Int, columnIndex:Int) = tableData(rowIndex)(columnIndex)
      }
    }
    val jTable = new JTable(generateTableModel(table)) with UpdateableNamedQuantityComponent {
      setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)
      setBorder(MatteBorder(1, 1, 0, 0, TableGridColour))
      setCellSelectionEnabled(true)

      def updateExtraInfo(newFI:ExtraFormatInfo) {
        val table0 = func.parameters.map(row(_, newFI))
        setModel(generateTableModel(table0))
        resizeTableColumnsToFit(this, PivotCellRenderer.MonoSpacedFont)
      }
    }
    allPanels += jTable
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
      val p = panel(f, fi)
      allPanels += p
      add(p, "gapleft 0, ay top")
    })
  }

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    funcLabel.tooltip = quantityText(func.result, newFI)
    allPanels.foreach(_.updateExtraInfo(newFI))
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

class BinOpNamedQuantityPanel(binOp:BinOpNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0", "[p]0[p][p][p]0[p]", "[2lp]0[p]0[2lp]") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val leftPanel = panel(binOp.lhs, fi)
  val binOpLabel = label(binOp.op, quantityText(binOp.result, fi))
  val rightPanel = panel(binOp.rhs, fi)

  add(new Brace(true), "growy, spany")
  add(new Brace(false), "skip 3, growy, spany, wrap")
  add(leftPanel, "ay top, skip 1")
  add(binOpLabel, "ay top")
  add(rightPanel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    binOpLabel.tooltip = quantityText(binOp.result, newFI)
    leftPanel.updateExtraInfo(newFI)
    rightPanel.updateExtraInfo(newFI)
  }
}

class NegateNamedQuantityPanel(negate:NegateNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0", "[p]0[p]") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val negativeLabel = label("-", quantityText(negate.quantity, fi))
  val negativePanel = panel(negate.qty, fi)
  add(negativeLabel, "ay top")
  add(negativePanel)

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    negativeLabel.tooltip = quantityText(negate.quantity, newFI)
    negativePanel.updateExtraInfo(newFI)
  }
}

class InvertNamedQuantityPanel(invert:InvertNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0", "[p]0[p]0[p]0[p]", "[2lp]0[p]0[2lp]") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val invertLabel = label("1/", quantityText(invert.quantity, fi))
  val invertPanel = panel(invert.qty, fi)
  add(new Brace(true), "growy, spany")
  add(new Brace(false), "skip 3, growy, spany, wrap")
  add(invertLabel, "ay top, skip 1")
  add(invertPanel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    invertLabel.tooltip = quantityText(invert.quantity, newFI)
    invertPanel.updateExtraInfo(newFI)
  }
}

class RoundedNamedQuantityPanel(round:RoundedNamedQuantity, fi:ExtraFormatInfo) extends MigPanel("insets 0", "[p]0", "[2lp]0[p]0[2lp]") with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val roundLabel = label("Round", quantityText(round.quantity, fi))
  val roundPanel = panel(round.orig, fi)

  add(new Brace(true), "skip 1, spany, growy")
  add(new Brace(false), "skip 4, spany, growy, wrap")
  add(roundLabel, "gapright 2lp, ay top")
  add(roundPanel, "skip 1, ay top")
  add(label(", " + round.dp.toString), "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    roundLabel.tooltip = quantityText(round.quantity, newFI)
    roundPanel.updateExtraInfo(newFI)
  }
}

class TopNamedQuantityComponent(quantity:SimpleNamedQuantity, formatInfo:ExtraFormatInfo=PivotFormatter.DefaultExtraFormatInfo) extends MigPanel with UpdateableNamedQuantityComponent {
  background = ExplanationPanelBackgroundColour
  val (panelToAdd, quantityValue) = quantity.quantity match {
    case nq:NamedQuantity => (new ExpandCollapsePanel(quantity, formatInfo), nq.quantity)
    case q:Quantity => (new QuantityPanel(q, formatInfo), q)
  }
  val valueLabel = label(quantityText(quantityValue, formatInfo))

  add(panelToAdd)
  add(label("="), "ay top")
  add(valueLabel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    valueLabel.text = quantityText(quantityValue, newFI)
    panelToAdd.updateExtraInfo(newFI)
  }
}

trait UpdateableNamedQuantityComponent {
  def updateExtraInfo(newFI:ExtraFormatInfo)
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

case class UnderLineDashedBorder(colour0:Color=LShape.colour) extends AbstractBorder {
  override def paintBorder(c:java.awt.Component, g:Graphics, x:Int, y:Int, width:Int, height:Int) {
    val g2 = g.asInstanceOf[Graphics2D]

    val s = c.getSize
    val w = s.width - 1
    val h = s.height - 1

    g2.setColor(colour0)
    g2.setStroke(stroke)
    g2.drawLine(2,h,w,h)
  }

  override def getBorderInsets(c:java.awt.Component, insets:Insets) = {
    insets.left = 0
    insets.right = 0
    insets.top = 0
    insets.bottom = 0
    insets
  }
  override def getBorderInsets(c:java.awt.Component) = new Insets(0,0,0,0)
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