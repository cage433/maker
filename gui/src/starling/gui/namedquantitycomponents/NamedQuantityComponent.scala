package starling.gui.namedquantitycomponents

import starling.browser.common.GuiUtils._
import swing.Swing._
import starling.quantity._
import starling.pivot.view.swing.PivotCellRenderer
import javax.swing.border.AbstractBorder
import swing.Label
import java.awt.{Color, Cursor, BasicStroke, Graphics, Insets, Graphics2D, Dimension}
import javax.swing.table.{DefaultTableCellRenderer, AbstractTableModel}
import starling.pivot.{QuantityLabelPivotFormatter, PivotFormatter, ExtraFormatInfo}
import collection.mutable.ListBuffer
import javax.swing.JTable
import org.jdesktop.swingx.JXTable
import starling.pivot.view.swing.{PivotJTable, PivotCellRenderer}
import org.jdesktop.swingx.decorator.{HighlightPredicate, ColorHighlighter}
import swing.event.{MouseClicked, MousePressed}
import starling.browser.common.{RoundedBorder, RoundedBackground, MigPanel}

class NamedQuantityComponentLabel(text0:String, tooltip0:String=null) extends Label {
  font = PivotCellRenderer.MonoSpacedFont
  text = text0
  tooltip = tooltip0
  if (text0.startsWith("(")) {
    foreground = Color.RED
  }
}

object NamedQuantityComponentHelper {
  def panel(namedQuantity:NamedQuantity, fi:ExtraFormatInfo, depth:Int):MigPanel with UpdateableNamedQuantityComponent = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi, depth)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi, depth)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi, depth - 1)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi, depth)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi, depth)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi, depth)
      case _ => new ExpandCollapsePanel(namedQuantity, fi, depth)
    }
  }

  def backColour(depth:Int) = {
    if ((depth % 2) == 0) {
      new Color(235,241,222) // GREENY
    } else {
      new Color(200,214,230) // BLUEY
    }
  }

  def label(text0:String, tooltip0:String=null) = new NamedQuantityComponentLabel(text0, tooltip0)

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

  def quantityText(q:Quantity, fi:ExtraFormatInfo) = {
    if (fi.decimalPlaces.unlimitedOnExplainScreen) {
      q.toStringAllDecimalPlaces()
    } else {
      QuantityLabelPivotFormatter.format(q, fi).text
    }
  }
}
import NamedQuantityComponentHelper._

object ExpandCollapseState {
  val Blank = ExpandCollapseState(None, Nil)
}
case class ExpandCollapseState(expanded:Option[Boolean], children:List[ExpandCollapseState])

class ExpandCollapsePanel(namedQuantity:NamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel with RoundedBackground with UpdateableNamedQuantityComponent {
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  private var expanded = false

  val nextDepth = depth + 1

  var expandable = true
  val expandedPanel = {
    namedQuantity match {
      case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi, nextDepth)
      case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi, nextDepth)
      case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi, depth)
      case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi, nextDepth)
      case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi, nextDepth)
      case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi, nextDepth)
      case nq:NamedQuantity => {
        nq.quantity match {
          case binOp:BinOpNamedQuantity => new BinOpNamedQuantityPanel(binOp, fi, nextDepth)
          case func:FunctionNamedQuantity if func.custom => new FunctionNamedQuantityPanel(func, fi, nextDepth)
          case func:FunctionNamedQuantity if !func.custom => new VerticalFunctionNamedQuantityPanel(func, fi, depth)
          case negate:NegateNamedQuantity => new NegateNamedQuantityPanel(negate, fi, nextDepth)
          case invert:InvertNamedQuantity => new InvertNamedQuantityPanel(invert, fi, nextDepth)
          case round:RoundedNamedQuantity => new RoundedNamedQuantityPanel(round, fi, nextDepth)
          case nq0:NamedQuantity => new ExpandCollapsePanel(nq0, fi, nextDepth)
          case q:Quantity => {
            expandable = false
            new Label("") with UpdateableNamedQuantityComponent{
              def updateExtraInfo(newFI: ExtraFormatInfo) {}
              def depth = 0
              def expandCollapseState = ExpandCollapseState.Blank
              def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {}
            }}
        }
      }
    }
  }
  lazy val lShapePanel = new LShape

  val label = new Label {
    text = generateText(fi)
    tooltip = quantityText(namedQuantity, fi)
    font = PivotCellRenderer.MonoSpacedFont
    if (expandable) {
      cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
      border = UnderLineDashedBorder()
    }

    def generateText(newFI:ExtraFormatInfo) = {
      val s = "   " + quantityText(namedQuantity, newFI)
      "<html>" + namedQuantity.name + "<br>&nbsp=" + s + "</html>"
    }
  }

  add(label)

  reactions += {case MousePressed(`label`,_,_,_,false) if expandable => {expandCollapse()}}
  listenTo(label.mouse.clicks)

  def expandCollapse() {
    if (expanded) {
      remove(expandedPanel)
      remove(lShapePanel)
    } else {
      val extraLayoutInfo = if (expandedPanel.isInstanceOf[QuantityPanel] || expandedPanel.isInstanceOf[VerticalFunctionNamedQuantityPanel]) {
        ""
      } else {
        ", gaptop 3"
      }
      if (expandable) {
        add(lShapePanel, "newline 0, split, spanx, gapright 0, ay top")
      }
      add(expandedPanel, "gapleft 0" + extraLayoutInfo)
    }
    expanded = !expanded
    revalidate()
    repaint()
  }
    
  def updateExtraInfo(newFI:ExtraFormatInfo) {
    label.tooltip = quantityText(namedQuantity, newFI)
    label.text = label.generateText(newFI)
    expandedPanel.updateExtraInfo(newFI)
  }

  expandCollapse()

  def expandCollapseState = {
    if (expandable) ExpandCollapseState(Some(expanded), expandedPanel.expandCollapseState :: Nil) else ExpandCollapseState.Blank
  }
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    if (expandable && expandCollapseState.expanded.isDefined) {
      if (expanded != expandCollapseState.expanded.get) {
        expandCollapse()
      }
    }
    if (expandCollapseState.children.nonEmpty) {
      expandedPanel.applyExpandCollapseState(expandCollapseState.children.head)
    }
  }
}

class QuantityPanel(quantity:Quantity, fi:ExtraFormatInfo, val depth:Int) extends Label with UpdateableNamedQuantityComponent {
  text = quantityText(quantity, fi)
  font = PivotCellRenderer.MonoSpacedFont
  if (text.startsWith("(")) {
    foreground = Color.RED
  }

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    text = quantityText(quantity, newFI)
  }
  def expandCollapseState = ExpandCollapseState.Blank
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {}
}

class FunctionNamedQuantityPanel(func:FunctionNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel with RoundedBackground with UpdateableNamedQuantityComponent {
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  val funcLabel = label(func.functionName, quantityText(func.result, fi))
  val allPanels = new ListBuffer[UpdateableNamedQuantityComponent]()

  add(funcLabel, "gapright 2lp, ay top")
  val holderPanel = new MigPanel("insets 0") {
    val nextDepth = depth + 1
    func.parameters.zipWithIndex.foreach{case (f,i) => {
      if (i != 0) {
        add(label(","), "ay top")
      }
      val p = panel(f, fi, nextDepth)
      allPanels += p
      add(p, "ay top")
    }}
  }
  add(holderPanel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    funcLabel.tooltip = quantityText(func.result, newFI)
    allPanels.foreach(_.updateExtraInfo(newFI))
  }

  def expandCollapseState = ExpandCollapseState(None, allPanels.map(_.expandCollapseState).toList)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    expandCollapseState.children.zip(allPanels).foreach{case (state, comp) => comp.applyExpandCollapseState(state)}
  }
}

class VerticalFunctionNamedQuantityPanel(func:FunctionNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel("insets 0, gap 0") with UpdateableNamedQuantityComponent {
  background = backColour(depth)

  val funcPanel = new MigPanel("insets 0, gap 0") {
    background = VerticalFunctionNamedQuantityPanel.this.background

    val qt = quantityText(func.result, fi)
    val label0 = label(qt)
    val funcLabel = label(func.functionName + " = ", qt)
    add(funcLabel)
    add(label0)
  }
  val allPanels = new ListBuffer[UpdateableNamedQuantityComponent]()

  add(funcPanel, "ay top")

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

    val negativeHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue.toString.startsWith("(")
      }
    })
    negativeHighlighter.setForeground(Color.RED)
    negativeHighlighter.setSelectedForeground(Color.RED)

    val lShape = new LShape {
      visible = false
    }

    val jTable = new JXTable(generateTableModel(table)) with UpdateableNamedQuantityComponent {
      setVisible(false)
      val depth = VerticalFunctionNamedQuantityPanel.this.depth
      setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)
      setBorder(MatteBorder(1, 1, 0, 0, TableGridColour))
      setCellSelectionEnabled(true)
      setRowHeight(PivotJTable.RowHeight)
      addHighlighter(negativeHighlighter)

      def updateExtraInfo(newFI:ExtraFormatInfo) {
        val table0 = func.parameters.map(row(_, newFI))
        setModel(generateTableModel(table0))
        resizeTableColumnsToFit(this, PivotCellRenderer.MonoSpacedFont)
      }
      def expandCollapseState = ExpandCollapseState(Some(isVisible), Nil)
      def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
        val visible = expandCollapseState.expanded.getOrElse(false)
        setVisible(visible)
        lShape.visible = visible
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

    val detailsButtonPanel = new MigPanel("insets 1 2 1 2") with RoundedBackground {
      cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
      background = Color.BLUE
      border = RoundedBorder(Color.WHITE)

      val l = new Label("Details") {
        foreground = Color.WHITE
      }
      add(l)
      reactions += {
        case MouseClicked(_,_,_,_,_) => {
          val v = !jTable.isVisible
          jTable.setVisible(v)
          lShape.visible = v
        }
      }
      listenTo(mouse.clicks)
    }

    add(detailsButtonPanel, "gapleft 6, gaptop 1, wrap")
    add(lShape, "split, spanx, ay top, gapright 0, hidemode 3")
    add(jTable, "hidemode 3")
  } else {
    func.parameters.foreach(f => {
      add(new LShape, "split, spanx, ay top, gapright 0, newline")
      val p = panel(f, fi, depth)
      allPanels += p
      add(p, "gapleft 0, ay top, gaptop 1")
    })
  }

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    funcPanel.label0.text = quantityText(func.result, newFI)
    funcPanel.funcLabel.tooltip = quantityText(func.result, newFI)
    allPanels.foreach(_.updateExtraInfo(newFI))
  }

  def expandCollapseState = ExpandCollapseState(None, allPanels.map(_.expandCollapseState).toList)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    expandCollapseState.children.zip(allPanels).foreach{case (state, comp) => comp.applyExpandCollapseState(state)}
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

class BinOpNamedQuantityPanel(binOp:BinOpNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel with RoundedBackground with UpdateableNamedQuantityComponent {
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  val nextDepth = depth + 1
  val leftPanel = panel(binOp.lhs, fi, nextDepth)
  val binOpLabel = label(binOp.op, quantityText(binOp.result, fi))
  val binOpPanel = new MigPanel("insets 0") with RoundedBackground {
    background = Color.RED
    border = RoundedBorder(Color.WHITE)
    binOpLabel.foreground = Color.WHITE
    binOpLabel.font = binOpLabel.font.deriveFont(java.awt.Font.BOLD, binOpLabel.font.getSize2D + 10.0f)
    binOpLabel.verticalAlignment = swing.Alignment.Center
    add(binOpLabel, "push, al center")
    val w0 = preferredSize.width
    val h0 = preferredSize.height
    val max = math.max(w0, h0)
    preferredSize = new Dimension(max, max)
  }
  val rightPanel = panel(binOp.rhs, fi, nextDepth)

  add(leftPanel, "ay top")
  add(binOpPanel, "ay top")
  add(rightPanel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    binOpLabel.tooltip = quantityText(binOp.result, newFI)
    leftPanel.updateExtraInfo(newFI)
    rightPanel.updateExtraInfo(newFI)
  }

  def expandCollapseState = ExpandCollapseState(None, List(leftPanel.expandCollapseState, rightPanel.expandCollapseState))
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    expandCollapseState.children match {
      case state1 :: state2 :: Nil => {
        leftPanel.applyExpandCollapseState(state1)
        rightPanel.applyExpandCollapseState(state2)
      }
      case _ =>
    }
  }
}

class NegateNamedQuantityPanel(negate:NegateNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel("" , "[p]0[p]") with RoundedBackground with UpdateableNamedQuantityComponent {
  val nextDepth = depth + 1
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  val negativeLabel = label("-", quantityText(negate.quantity, fi))
  val negativePanel = panel(negate.qty, fi, nextDepth)
  add(negativeLabel, "ay top")
  add(negativePanel)

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    negativeLabel.tooltip = quantityText(negate.quantity, newFI)
    negativePanel.updateExtraInfo(newFI)
  }

  def expandCollapseState = ExpandCollapseState(None, negativePanel.expandCollapseState :: Nil)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    if (expandCollapseState.children.nonEmpty) {
      negativePanel.applyExpandCollapseState(expandCollapseState.children.head)
    }
  }
}

class InvertNamedQuantityPanel(invert:InvertNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel with RoundedBackground with UpdateableNamedQuantityComponent {
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  val nextDepth = depth + 1
  val invertLabel = label("1/", quantityText(invert.quantity, fi))
  val invertPanel = panel(invert.qty, fi, nextDepth)
  add(invertLabel, "ay top")
  add(invertPanel, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    invertLabel.tooltip = quantityText(invert.quantity, newFI)
    invertPanel.updateExtraInfo(newFI)
  }

  def expandCollapseState = ExpandCollapseState(None, invertPanel.expandCollapseState :: Nil)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    if (expandCollapseState.children.nonEmpty) {
      invertPanel.applyExpandCollapseState(expandCollapseState.children.head)
    }
  }
}

class RoundedNamedQuantityPanel(round:RoundedNamedQuantity, fi:ExtraFormatInfo, val depth:Int) extends MigPanel with RoundedBackground with UpdateableNamedQuantityComponent {
  background = backColour(depth)
  border = RoundedBorder(background.darker())
  val nextDepth = depth + 1
  val roundLabel = label("Round", quantityText(round.quantity, fi))
  val roundPanel = panel(round.orig, fi, nextDepth)

  add(roundLabel, "gapright 2lp, ay top")
  add(roundPanel, "ay top")
  add(label(", " + round.dp.toString), "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    roundLabel.tooltip = quantityText(round.quantity, newFI)
    roundPanel.updateExtraInfo(newFI)
  }

  def expandCollapseState = ExpandCollapseState(None, roundPanel.expandCollapseState :: Nil)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    if (expandCollapseState.children.nonEmpty) {
      roundPanel.applyExpandCollapseState(expandCollapseState.children.head)
    }
  }
}

class TopNamedQuantityComponent(quantity:NamedQuantity, formatInfo:ExtraFormatInfo=PivotFormatter.DefaultExtraFormatInfo) extends MigPanel with UpdateableNamedQuantityComponent {
  val depth = -1
  background = ExplanationPanelBackgroundColour
  val panelToAdd = quantity.quantity match {
    case nq:NamedQuantity => {
      val panel = new ExpandCollapsePanel(quantity, formatInfo, 0) {
        background = new Color(241,239,239)
        border = RoundedBorder(background.darker())
      }
      panel
    }
    case q:Quantity => new QuantityPanel(q, formatInfo, 0)
  }

  add(panelToAdd, "ay top")

  def updateExtraInfo(newFI:ExtraFormatInfo) {
    panelToAdd.updateExtraInfo(newFI)
  }

  def expandCollapseState = ExpandCollapseState(None, panelToAdd.expandCollapseState :: Nil)
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState) {
    if (expandCollapseState.children.nonEmpty) {
      panelToAdd.applyExpandCollapseState(expandCollapseState.children.head)
    }
  }
}

trait UpdateableNamedQuantityComponent {
  def updateExtraInfo(newFI:ExtraFormatInfo)
  def expandCollapseState:ExpandCollapseState
  def applyExpandCollapseState(expandCollapseState:ExpandCollapseState)
  def depth:Int
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
//    val volume = Quantity(100.0, UOM.BBL).named("Volume")
    val discount = new Quantity(0.9).named("Discount")
//    val priceTimesVolume = ((price - strike) * volume) * discount

    val func = FunctionNamedQuantity("Sum", List(price.negate, strike.invert), price + strike, true) * discount.round(3)

//    val topQuantity = SimpleNamedQuantity("P&L", priceTimesVolume)
    val topQuantity = SimpleNamedQuantity("P&L", func)

    onEDT(showInFrame(new TopNamedQuantityComponent(topQuantity), pack0 = false))
  }
}
