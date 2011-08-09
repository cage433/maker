package starling.pivot.view.swing

import fieldchoosers.{RowComponent, FilterComponent, FieldListComponent}
import scala.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import starling.pivot.model._
import starling.pivot._
import controller.PivotTableConverter
import starling.pivot.FieldChooserType._
import org.jdesktop.swingx.decorator.{HighlightPredicate}
import org.jdesktop.swingx.JXTable
import swing.event.{Event, MouseClicked, KeyPressed, KeyReleased}
import starling.rmi.PivotData
import scala.swing.Swing._
import starling.gui.GuiUtils._
import starling.gui.{GuiUtils, OldPageData, ComponentRefreshState, StarlingIcons}
import java.awt.datatransfer.StringSelection
import java.awt.event._
import starling.gui.api.ReportSpecificOptions
import starling.gui.pages.ConfigPanels
import collection.immutable.List
import starling.utils.ImplicitConversions._
import collection.mutable.{ListBuffer, HashMap}
import org.jdesktop.animation.timing.{TimingTargetAdapter, Animator}
import java.awt.{Container, Graphics2D, Dimension, Color, GradientPaint, Cursor, AWTEvent, Toolkit, KeyboardFocusManager, Component => AWTComp}
import javax.swing._
import starling.pivot.view.swing.PivotTableType._
import starling.pivot.HiddenType._

object PivotTableView {
  def createWithLayer(data:PivotData, otherLayoutInfo:OtherLayoutInfo, browserSize:Dimension,
                      configPanels:Option[ConfigPanels], extraFormatInfo:ExtraFormatInfo, edits:PivotEdits, embedded:Boolean = true) = {
    val viewUI = new PivotTableViewUI
    val view = new PivotTableView(data, otherLayoutInfo, browserSize, configPanels, extraFormatInfo, embedded, viewUI, edits)
    val layer = new SXLayerScala(view, viewUI)
    layer
  }

  val RefreshFadeTime = 10000
}

case class FieldsChangedEvent(pivotFieldState:PivotFieldsState) extends Event
case class TableDoubleClickEvent(filterFields:Seq[(Field,Selection)] , drillDownFields:Seq[(Field,Selection)], controlDown:Boolean) extends Event
case class FullScreenSelectedEvent(currentState:HiddenType, newState:HiddenType, currentFrozen:Boolean) extends Event
case class ShowErrorsEvent(errors:Set[StackTrace]) extends Event
case class PivotEditsUpdatedEvent(edits:PivotEdits, source:PivotJTable) extends Event
case object SavePivotEdits extends Event
case class FieldPanelEvent(collapse:Boolean) extends Event
case class CollapsedStateUpdated(rowCollapsedState:Option[CollapsedState]=None, columnCollapsedState:Option[CollapsedState]=None) extends Event
case class GridSelectionEvent(selection:Option[(String,Boolean)]) extends Event

class PivotTableView(data:PivotData, otherLayoutInfo:OtherLayoutInfo, browserSize0:Dimension,
                     configPanels:Option[ConfigPanels], extraFormatInfo:ExtraFormatInfo, embedded:Boolean,
                     viewUI:PivotTableViewUI, edits:PivotEdits)
        extends MigPanel("hidemode 2, insets 0, gap 0") {
  private var browserSize = browserSize0
  private val model = new starling.pivot.model.PivotTableModel(data)
  private var reverseToolBarState:()=>Unit = null

  private val fieldListComponent = new FieldListComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable.editableInfo)
  private val columnAndMeasureComponent = new ColumnAndMeasureComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable.editableInfo)
  private val filterComponent = new FilterComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable.editableInfo)
  private val rowComponent = new RowComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable.editableInfo)

  private val allDropTargets = List(fieldListComponent, columnAndMeasureComponent, filterComponent, rowComponent)

  private var draggedField0:Field = null
  def draggedField = draggedField0
  def draggedField_=(f:Field) {draggedField0 = f}
  private var fieldBeingDragged0 = false
  def fieldBeingDragged = fieldBeingDragged0
  def fieldBeingDragged_=(b:Boolean) {
    if (!fieldBeingDragged0 && b) {
      allDropTargets.foreach(_.show(draggedField0))
    }
    fieldBeingDragged0 = b
  }
  private var mouseDown0 = false
  def mouseDown = mouseDown0
  def mouseDown_=(b:Boolean) {mouseDown0 = b}
  def drag = mouseDown0

  private def hideDropTargets() {allDropTargets.foreach(_.hide())}

  def fieldDropped(field:Field, from:FieldChooserType, screenPoint:Point):Boolean = {
    val noEffect = if (!model.getFields(FieldList).fields.contains(field) && fieldListComponent.dropBounds(field).exists(_.contains(screenPoint))) {
      model.publishFieldStateChange(field, 0, from, FieldList)
    } else if (columnAndMeasureComponent.dropBounds(field).exists(_.contains(screenPoint))) {
      val newColumnStructure = columnAndMeasureComponent.newColumnStructure(screenPoint, field)
      model.publishFieldStateChange(field, newColumnStructure, from)
    } else if (filterComponent.dropBounds(field).exists(_.contains(screenPoint))) {
      val pos = filterComponent.indexOfDrop(screenPoint, field)
      model.publishFieldStateChange(field, pos, from, Filter)
    } else if (rowComponent.dropBounds(field).exists(_.contains(screenPoint))) {
      val pos = rowComponent.indexOfDrop(screenPoint, field)
      model.publishFieldStateChange(field, pos, from, Rows)
    } else {
      true
    }
    if (noEffect) {
      hideDropTargets()
    }
    noEffect
  }

  def fieldDoubleClicked(field:Field, from:FieldChooserType) {
    viewUI.resetImageProperties()
    if (from != FieldList) {
      model.publishFieldStateChange(field, 0, from, FieldList)
    } else {
      if (model.isMeasureField(field)) {
        columnAndMeasureComponent.fieldGoingToBeAddedToTheEnd()
        model.publishFieldStateChange(field, model.columns.addDataField(field), from)
      } else {
        rowComponent.fieldGoingToBeAddedToTheEnd()
        model.publishFieldStateChange(field, rowComponent.numberOfFields, from, Rows)
      }
    }
  }

  private val chooserLayerScrollPane = new ScrollPane(fieldListComponent) {
    border = MatteBorder(1,1,1,0,BorderColour)
    verticalScrollBar.preferredSize = new Dimension(0,0)
    verticalScrollBar.unitIncrement = 10
    horizontalScrollBar.unitIncrement = 10
    peer.getVerticalScrollBar.addAdjustmentListener(new AdjustmentListener{
      def adjustmentValueChanged(e: AdjustmentEvent) {
        viewUI.resetImageProperties()
        fieldListComponent.reset()
      }
    })
  }

  private val fakeChooserScrollPane = new JScrollPane
  private val fakeVChooserScrollBar = fakeChooserScrollPane.getVerticalScrollBar
  fakeVChooserScrollBar.setUnitIncrement(UnitIncrement)
  fakeVChooserScrollBar.setBlockIncrement(BlockIncrement)
  fakeVChooserScrollBar.setModel(chooserLayerScrollPane.verticalScrollBar.peer.getModel)
  private val fakeVChooserScrollBarHolder = new MigPanel("insets 0") {
    border = MatteBorder(1,0,0,1,BorderColour)
    add(fakeVChooserScrollBar, "push,grow")
  }

  def getFieldChooser(fieldChooserType:FieldChooserType) = Rows
  
  def filtersWithSome = model.filtersWithSome

  private val FieldListText = "Field List"

  private val fieldChooserPanelHolder = new MigPanel("insets 0") {
    def setComponent(comp:Component) {
      removeAll
      add(comp, "push, grow")
      PivotTableView.this.revalidate
      PivotTableView.this.repaint

      minimumSize = new Dimension(preferredSize.width + 1, 10)
    }
  }

  private val fieldPanel = new MigPanel("insets 0", "[p]0[p]", "[p]0[p]0[p]") {
    if (embedded) {
      border = MatteBorder(1, 0, 0, 0, BorderColour)
    } 
    val listHeaderLabel = new Label(FieldListText + "                      ") {
      foreground = Color.WHITE
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val listHeaderLabelPanel = new MigPanel("insets 2lp") {
      border = MatteBorder(0, 0, 0, 1, BorderColour)
      add(listHeaderLabel, "gapbefore 1lp")

      override def paintComponent(g2:Graphics2D) = {
        val oldPaint = g2.getPaint
        g2.setPaint(new GradientPaint(0, 0, new Color(145, 181, 255), 0, size.height, new Color(96, 123, 183)))
        g2.fillRect(0, 0, size.width, size.height)
        g2.setPaint(oldPaint)
      }
    }
    val listHeaderPanel = new MigPanel("insets 0", "[p]0[p]") {
      border = MatteBorder(0, 1, 0, 1, BorderColour)
      val fixedImagePanel = new FixedImagePanel(StarlingIcons.im("/icons/hideSideLeft.png"))
      val fixedImageHolderPanel = new MigPanel("insets 2lp") {
        opaque = false
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
        add(fixedImagePanel, "push, grow")
        reactions += {case MouseClicked(_,_,_,_,_) => PivotTableView.this.publish(FieldPanelEvent(true))}
        listenTo(mouse.clicks)
      }
      val gradientPanel = new MigPanel("insets 0") {
        override def paintComponent(g2:Graphics2D) = {
          val oldPaint = g2.getPaint
          g2.setPaint(new GradientPaint(0, 0, Color.WHITE, 0, size.height, new Color(179, 197, 231)))
          g2.fillRect(0, 0, size.width, size.height)
          g2.setPaint(oldPaint)
        }
        add(fixedImageHolderPanel, "push, grow")
      }
      add(listHeaderLabelPanel, "pushx, grow")
      add(gradientPanel, "ay center")
    }

    val filterPanel = new MigPanel("insets 2lp", "[p]0[p]0[p]") {
      border = MatteBorder(0, 1, 0, 1, BorderColour)
      val search = new FixedImagePanel(StarlingIcons.im("/icons/16x16find.png"))
      val searchHolder = new MigPanel("insets 0") {
        border = MatteBorder(1, 1, 1, 0, BorderColour)
        add(search, "gapbefore 2lp, gapafter 2lp, align center center, push")
      }
      val textField = new TextField {
        override protected def paintBorder(g:Graphics2D) = {
          super.paintBorder(g)
          val width = size.width - 1
          val height = size.height - 2
          g.setColor(Color.WHITE)
          g.drawLine(width, 1, width, height)
          g.setColor(BorderColour.brighter)
          g.drawLine(0, 1, 0, height)
        }
      }
      val clearImage = new FixedImagePanel(StarlingIcons.im("/icons/closeHovered.png")) {
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
      }
      val clearImageHolder = new MigPanel("insets 0") {
        border = MatteBorder(1, 0, 1, 1, BorderColour)
        background = Color.WHITE
        add(clearImage, "align center center, push")
      }
      reactions += {
        case KeyPressed(`textField`, scala.swing.event.Key.Escape, _, _) => textField.text = ""
        case KeyReleased(`textField`, _, _, _) => {
          // Ensure the fields aren't being displayed here.
//          chooserPanel.resetImage
          fieldListComponent.setTextFilter(textField.text)
        }
        case MouseClicked(`clearImage`,_,_,_,_) => {
          textField.text = ""
          fieldListComponent.setTextFilter(textField.text)
        }
      }
      listenTo(textField.keys, clearImage.mouse.clicks)
      add(searchHolder, "grow")
      add(textField, "push, grow")
      add(clearImageHolder, "grow")
    }

    add(listHeaderPanel.peer, "spanx, growx, wrap")
    add(filterPanel.peer, "spanx, growx, wrap")
    add(chooserLayerScrollPane.peer, "grow, push")
    add(fakeVChooserScrollBarHolder.peer, "grow")

    override def paintComponent(g:Graphics2D) = {
      super.paintComponent(g)
      if (embedded) {
        g.setColor(BorderColour)
        val h = size.height - 1
        val w = size.width - 1
        g.drawLine(0, h, LeftPanelValue, h)
        g.drawLine(w, h, w - RightPanelValue, h)
      }
    }
  }

  def filterText_=(text:String) {
    fieldPanel.filterPanel.textField.text = text
    fieldListComponent.setTextFilter(text)
  }
  def filterText = fieldPanel.filterPanel.textField.text

  private val hiddenFieldPanelLeftInsets = if (embedded) "2lp" else "0"
  private val hiddenFieldPanel = new MigPanel("insets 2lp " + hiddenFieldPanelLeftInsets + " 0 2lp") {
    if (embedded) {
      border = MatteBorder(1, 1, 1, 1, BorderColour)
    } else {
      border = MatteBorder(0, 0, 0, 1, BorderColour)
    }
    val verticalButton = new VerticalButton(FieldListText) {
      setFocusable(false)
      addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) = {PivotTableView.this.publish(FieldPanelEvent(false))}
      })
    }
    add(verticalButton)
  }

  def getColScrollPos = columnAndMeasureScrollPane.peer.getViewport.getViewPosition.x
  def setColScrollPos(pos:Int) = columnAndMeasureScrollPane.peer.getViewport.setViewPosition(new Point(pos,0))
  def getRSScrollPos = reportSpecificOptionsScrollPane.peer.getViewport.getViewPosition.x
  def setRSScrollPos(pos:Int) = reportSpecificOptionsScrollPane.peer.getViewport.setViewPosition(new Point(pos,0))
  def getMainScrollPos = if (otherLayoutInfo.frozen) {
    mainTableScrollPane.getViewport.getViewPosition
  } else {
    fullTableScrollPane.getViewport.getViewPosition
  }
  def setMainScrollPos(pos:Point) = if (otherLayoutInfo.frozen) {
    mainTableScrollPane.getViewport.setViewPosition(pos)
  } else {
    fullTableScrollPane.getViewport.setViewPosition(pos)
  }
  def getSelectedCells = {
    if (otherLayoutInfo.frozen) {
      Right(mainTable.getSelectedCells, rowHeaderTable.getSelectedCells, colHeaderTable.getSelectedCells)
    } else {
      Left(fullTable.getSelectedCells)
    }
  }

  def setSelectedCells(cells:Either[List[(Int,Int)], (List[(Int,Int)],List[(Int,Int)],List[(Int,Int)])]) {

    def isCellVisible(t:JTable, sp:JScrollPane, r:Int, c:Int) = {
      val viewport = sp.getViewport
      val rect = t.getCellRect(r, c, true)
      val pt = viewport.getViewPosition
      rect.setLocation(rect.x - pt.x, rect.y - pt.y)
      new Rectangle(viewport.getExtentSize).contains(rect)
    }

    def doScrolling(t:JXTable, sp:JScrollPane, r:Int, c:Int) {
      onEDT(onEDT(onEDT({
        if (!isCellVisible(t, sp, r, c)) {
          t.scrollCellToVisible(r, c)
        }
      })))
    }

    cells match {
      case Left(sc) => {
        fullTable.setSelectedCells(sc)
        if (sc.size == 1) {
          val (r,c) = sc.head
          doScrolling(fullTable, fullTableScrollPane, r, c)
        }
        if (sc.nonEmpty) Some(fullTable) else None
      }
      case Right((m,r,c)) => {
        mainTable.setSelectedCells(m)
        rowHeaderTable.setSelectedCells(r)
        colHeaderTable.setSelectedCells(c)
        if (m.nonEmpty) {
          if (m.size == 1) {
            val (r,c) = m.head
            doScrolling(mainTable, mainTableScrollPane, r, c)
          }
        } else if (r.nonEmpty) {
          if (r.size == 1) {
            val (row,c) = r.head
            doScrolling(rowHeaderTable, rowHeaderTableScrollPane, row, c)
          }
        }
      }
    }
  }

  def updateFocusBasedOnCellSelection() {
    getSelectedCells match {
      case Left(_) => fullTable.requestFocusInWindow()
      case Right((m,r,c)) => {
        if (m.nonEmpty) {
          mainTable.requestFocusInWindow()
        } else if (r.nonEmpty) {
          rowHeaderTable.requestFocusInWindow()
        } else if (c.nonEmpty) {
          colHeaderTable.requestFocusInWindow()
        }
      }
    }
  }

  def configPanelState = configTabbedPane match {
    case None => None
    case Some(tp) => Some(tp.state)
  }
  def configPanelState_=(s:Option[NTabbedPaneState]) {
    s match {
      case None =>
      case Some(state) => configTabbedPane match {
        case None =>
        case Some(tp) => tp.state = state
      }
    }
  }

  private val formulaBar = new MigPanel("insets 0, gap 0") {
    var expanded = false
    val iconHolder = new MigPanel("insets 2lp 3lp 0 3lp") {
      border = MatteBorder(1,0,1,0, BorderColour)
      add(new Label {
        icon = StarlingIcons.icon("/icons/16x16_info.png")
      }, "push, ay top")
    }

    private val onelineText = new TextField {
      editable = false
      background = Color.WHITE
      maximumSize = new Dimension(Integer.MAX_VALUE, preferredSize.height)
    }

    private val textHolder = new MigPanel("insets 0") {
      def update(c:Component) {
        removeAll
        add(c, "push,grow")
        revalidate
        repaint
      }
    }

    private val multilineText = new TextArea {
      editable = false
      background = Color.WHITE
      lineWrap = true
      wordWrap = true
      rows = 3
      border = CompoundBorder(LineBorder(BorderColour), EmptyBorder(2))
      minimumSize = preferredSize
    }

    def setText(t:String, summary:Boolean) {
      onelineText.text = t
      onelineText.peer.setHorizontalAlignment(if (summary) SwingConstants.RIGHT else SwingConstants.LEFT)
      multilineText.text = t
      if (summary) {
        if (expanded) shrink
        expandButtonHolder.expandButton.visible = false
      } else {
        expandButtonHolder.expandButton.visible = true
      }
    }

    def expand {
      expanded = true
      expandButtonHolder.expandButton.icon = StarlingIcons.icon("/icons/scroll_up.png")
      textHolder.update(multilineText)
    }

    def shrink {
      expanded = false
      expandButtonHolder.expandButton.icon = StarlingIcons.icon("/icons/scroll_down.png")
      textHolder.update(onelineText)
    }

    val expandButtonHolder = new MigPanel("insets 3lp 0 0 0") {
      border = MatteBorder(1,0,1,0,BorderColour)
      val expandButton = new Label {
        icon = StarlingIcons.icon("/icons/scroll_down.png")
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
        reactions += {
          case scala.swing.event.MousePressed(_,_,_,_,_) => {
            if (expanded) {
              shrink
            } else {
              expand
            }
          }
        }
        listenTo(mouse.clicks)
      }
      add(expandButton, "push, ay top")
    }

    shrink

    add(iconHolder, "grow")
    add(textHolder, "push,grow")
    add(expandButtonHolder, "grow")
  }

  private val toolbarPanel = new MigPanel("insets 0") {
    visible = false
    if (embedded) {
      border = MatteBorder(1, 0, 0, 0, BorderColour)
    }
  }

  reactions += {
    case GridSelectionEvent(selection) => {
      val (text, summary) = selection.getOrElse(("", false))
      formulaBar.setText(text, summary)
    }
  }

  def setCustomToolBar(component:Component, reverseToolBarState:()=>Unit) {
    toolbarPanel.peer.add(component.peer, "pushx, growx")
    if (otherLayoutInfo.hiddenType != AllHidden) {
      toolbarPanel.visible = true
    }
    this.reverseToolBarState = reverseToolBarState
  }

  private val columnAndMeasureScrollPane = PivotTableViewHelper.generateScrollableFieldChooser(columnAndMeasureComponent, columnAndMeasureComponent, this)

  if (!otherLayoutInfo.frozen) {
    rowComponent.opaque = true
    rowComponent.background = PivotTableBackgroundColour
    rowComponent.border = BorderFactory.createMatteBorder(0,0,1,1,GuiUtils.BorderColour)
  }
  val sizerPanel = new FlowPanel {
    background = GuiUtils.PivotTableBackgroundColour
  }

  private val viewConverter = PivotTableConverter(otherLayoutInfo, data.pivotTable, extraFormatInfo, data.pivotFieldsState)
  private val (rowHeaderData, colHeaderData, mainData, colUOMs) = viewConverter.allTableCellsAndUOMs

  private def resizeColumnHeaderAndMainTableColumns {
    tableModelsHelper.resizeColumnHeaderAndMainTableColumns(fullTable, mainTable, colHeaderTable,
      columnHeaderScrollPane, columnHeaderScrollPanePanel, mainTableScrollPane)
  }

  private def resizeRowHeaderTableColumns {
    tableModelsHelper.resizeRowHeaderColumns(fullTable, rowHeaderTable, rowComponent, data.pivotTable.rowFieldHeadingCount, sizerPanel, rowHeaderTableScrollPane)
    contentPanel.revalidate()
    contentPanel.repaint()
  }

  private def updatePivotEdits(edits0:PivotEdits, tableType:PivotTableType) {
    val t = tableType match {
      case Full => fullTable
      case RowHeader => rowHeaderTable
      case ColumnHeader => colHeaderTable
      case Main => mainTable
    }
    publish(PivotEditsUpdatedEvent(edits0, t))
  }

  private val tableModelsHelper = new PivotJTableModelHelper(mainData, data.pivotTable.editableInfo,
    rowHeaderData, colHeaderData, colUOMs, resizeColumnHeaderAndMainTableColumns, resizeRowHeaderTableColumns,
    data.pivotFieldsState, extraFormatInfo, edits, (edits0, tableType) => updatePivotEdits(edits0, tableType), data.pivotTable.formatInfo)

  def extraFormatInfoUpdated(extraFormatInfo:ExtraFormatInfo) {
    val newConverter = viewConverter.copy(extraFormatInfo = extraFormatInfo)
    val (newRowData,newColumnData,newMainTableCells,_) = newConverter.allTableCellsAndUOMs
    tableModelsHelper.setData(newRowData, newColumnData, newMainTableCells, extraFormatInfo)
  }

  // What I need to do here is loop through the row header table and decide which columns have collapsible elements. If a column does,
  // everything in that column needs to be indented.
  val numRows = rowHeaderData.length
  val numCols = rowHeaderData(0).length
  val indents = Array.fill(numCols)(false)
  for (col <- (0 until numCols)) {
    var notFound = true
    for (row <- (0 until numRows) if notFound) {
      if(rowHeaderData(row)(col).collapsible.isDefined) {
        indents(col) = true
        notFound = false
      }
    }
  }

  private var lastTableToHaveFocus:PivotJTable = null
  def copyReportToClip {
    val convertSelectedCellsToString = if (otherLayoutInfo.frozen) {
      if (lastTableToHaveFocus != null) {
        lastTableToHaveFocus.convertSelectedCellsToString
      } else {
        ""
      }
    } else {
      fullTable.convertSelectedCellsToString
    }
    val clip = Toolkit.getDefaultToolkit.getSystemClipboard
    val stringSelection = new StringSelection(convertSelectedCellsToString)
    clip.setContents(stringSelection, null)
  }

  def collapseOrExpandRow(path:List[AxisValue]) {
    val collapsed = viewConverter.collapsedRowState.collapsed(path)
    val newCollapsedRowState = if (collapsed) {
      // It is already collapsed so expand it.
      viewConverter.collapsedRowState.removeElement(CollapsedElement(path))
    } else {
      // We need to collapse it.
      viewConverter.collapsedRowState.addElement(CollapsedElement(path))
    }
    publish(CollapsedStateUpdated(rowCollapsedState = Some(newCollapsedRowState)))
  }

  def collapseOrExpandCol(path:List[AxisValue]) {
    val collapsed = viewConverter.collapsedColState.collapsed(path)
    val newCollapsedColState = if (collapsed) {
      viewConverter.collapsedColState.removeElement(CollapsedElement(path))
    } else {
      viewConverter.collapsedColState.addElement(CollapsedElement(path))
    }
    publish(CollapsedStateUpdated(columnCollapsedState = Some(newCollapsedColState)))
  }

  private val fullTable = new PivotJTable(tableModelsHelper.fullTableModel, this, model, indents)
  private val mainTable = new PivotJTable(tableModelsHelper.mainTableModel, this, model, indents)
  private val rowHeaderTable = new PivotJTable(tableModelsHelper.rowHeaderTableModel, this, model, indents)
  private val colHeaderTable = new PivotJTable(tableModelsHelper.colHeaderTableModel, this, model, indents)
  private val allTables = List(fullTable, mainTable, rowHeaderTable, colHeaderTable)
  mainTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = mainTable}})
  colHeaderTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = colHeaderTable}})
  rowHeaderTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = rowHeaderTable}})

  private var ignoreSelectionEvents = false

  class TableSelectionListener(table: PivotJTable, otherTablesToClearSelectionOn:PivotJTable*) extends ListSelectionListener {
    var lastEvent : Option[GridSelectionEvent] = None

    def valueChanged(e: ListSelectionEvent) = {
      if (!ignoreSelectionEvents) { //skip events generated by clearing the selection on other tables
        ignoreSelectionEvents = true
        otherTablesToClearSelectionOn.foreach{ t => {
          t.getColumnModel.getSelectionModel.clearSelection
          t.getSelectionModel.clearSelection
        } }
        ignoreSelectionEvents = false
        val selection:Option[(String,Boolean)] = table.getSelectedCells match {
          case Nil => None
          case (row, col) :: Nil => {
            table.getValueAt(row, col) partialMatch {
              case tc:TableCell => (tc.longText.getOrElse(tc.text), false)
              case ac:AxisCell => (ac.text, false)
            }
          }
          case many => {
            val values = many.flatMapO { case (row, col) => {
              table.getValueAt(row, col) partialMatchO {
                case tc:TableCell => tc.doubleValue
              }
            }}
            if (values.isEmpty) {
              None
            } else {
              val sum = values.sum
              val count = values.size
              val average = sum / count
              Some( (" Average: " + average + " | Count: " + count + " | Sum: " + sum, true) )
            }
          }
        }
        val event = GridSelectionEvent(selection)
        if (Some(event) != lastEvent) {
          publish(event)
          lastEvent = Some(event)
        }
      }
    }
  }

  {
    def listenForSelection(table: PivotJTable, otherTablesToClearSelectionOn:PivotJTable*) {
      val listener = new TableSelectionListener(table, otherTablesToClearSelectionOn : _ *)
      table.getColumnModel.getSelectionModel.addListSelectionListener(listener)
      table.getSelectionModel.addListSelectionListener(listener)
    }
    listenForSelection(fullTable)
    listenForSelection(mainTable, rowHeaderTable, colHeaderTable)
    listenForSelection(rowHeaderTable, mainTable, colHeaderTable)
    listenForSelection(colHeaderTable, rowHeaderTable, mainTable)
  }


  private val (fullTableScrollPane, fullHScrollBarHolder, fullVScrollBarHolder) = {
    val tableLayerUI = new UnfrozenTableLayerUI(rowComponent)
    val layer = new SXLayer(fullTable, tableLayerUI) {
      setLayerEventMask(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK
              //| AWTEvent.MOUSE_WHEEL_EVENT_MASK - mouse wheel event's will pass through
              | AWTEvent.KEY_EVENT_MASK | AWTEvent.FOCUS_EVENT_MASK)
    }.peer
    PivotTableViewHelper.generateScrollPaneHolders(layer)
  }
  if (embedded && otherLayoutInfo.hiddenType == AllHidden) {
    fullTableScrollPane.setBorder(MatteBorder(1,1,0,0,BorderColour))
    fullHScrollBarHolder.border = MatteBorder(0,1,1,0,BorderColour)
  }

  def giveDefaultFocus {
    if (otherLayoutInfo.frozen) {
      mainTable.requestFocusInWindow
    } else {
      fullTable.requestFocusInWindow
    }
  }

  private val (mainTableScrollPane, mainHScrollBarHolder, mainVScrollBarHolder) = {
    PivotTableViewHelper.generateScrollPaneHolders(mainTable)
  }
  mainTableScrollPane.setBorder(BorderFactory.createMatteBorder(1,1,0,0,BorderColour))

  private val rowHeaderTableScrollPane = new JScrollPane(rowHeaderTable)
  rowHeaderTableScrollPane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0))
  rowHeaderTableScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
  rowHeaderTableScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)

  private val rowHeaderTableScrollPanePanel = new MigPanel("insets 0", "[grow, right]") {
    add(rowHeaderTableScrollPane, "pushy, growy")
    border = BorderFactory.createMatteBorder(1, 0, 0, 0, BorderColour)
  }

  private val rowHeaderVerticalScrollBar = rowHeaderTableScrollPane.getVerticalScrollBar
  rowHeaderVerticalScrollBar.setPreferredSize(new Dimension(0, 0))
  rowHeaderVerticalScrollBar.setModel(mainTableScrollPane.getVerticalScrollBar.getModel)

  private val columnHeaderScrollPane = new JScrollPane(colHeaderTable)
  columnHeaderScrollPane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0))
  columnHeaderScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
  columnHeaderScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)

  private val columnHeaderScrollPanePanel = new MigPanel("insets 0", "", "[grow, bottom]") {
    add(columnHeaderScrollPane, "pushx,growx")
    border = BorderFactory.createMatteBorder(1, 1, 0, 0, BorderColour)
  }

  private val columnHeaderHorizontalScrollBar = columnHeaderScrollPane.getHorizontalScrollBar
  columnHeaderHorizontalScrollBar.setPreferredSize(new Dimension(0, 0))
  columnHeaderHorizontalScrollBar.setModel(mainTableScrollPane.getHorizontalScrollBar.getModel)

  tableModelsHelper.fullTableModel.fireTableStructureChanged
  tableModelsHelper.resizeRowHeaderColumns(fullTable, rowHeaderTable, rowComponent, data.pivotTable.rowFieldHeadingCount, sizerPanel, rowHeaderTableScrollPane)
  tableModelsHelper.resizeColumnHeaderAndMainTableColumns(fullTable, mainTable, colHeaderTable, columnHeaderScrollPane,
    columnHeaderScrollPanePanel, mainTableScrollPane)
  
  def setRefreshState(rState:PivotTableViewRefreshState) {
    // This is being set from previous component on refresh.
    refreshMainMap ++= rState.refreshCellMainMap
    refreshRowMap ++= rState.refreshCellRowMap
    refreshColMap ++= rState.refreshCellColMap
  }

  def getRefreshState = {
    def setFractions(map:HashMap[(Int, Int), RefreshedCell]) {
      for (index <- map.keySet) {
        val currentFraction = map(index).currentFraction + currentAnimatorFraction
        map(index).currentFraction = currentFraction
      }
    }
    setFractions(refreshRowMap)
    setFractions(refreshColMap)
    setFractions(refreshMainMap)

    PivotTableViewRefreshState(refreshRowMap,refreshColMap,refreshMainMap)
  }

  def getOldPageData = AbstractPivotComponentOldPageData(tableModelsHelper.rowHeaderData0, tableModelsHelper.colHeaderData0, tableModelsHelper.data0)

  private val refreshMainMap = new HashMap[(Int,Int),RefreshedCell]()
  private val refreshRowMap = new HashMap[(Int,Int),RefreshedCell]()
  private val refreshColMap = new HashMap[(Int,Int),RefreshedCell]()

  private var currentAnimatorFraction = 0.0f
  private val refreshMainHighlighter = new MapHighlighter(refreshMainMap)

  private def startAnimation {
    new Animator(PivotTableView.RefreshFadeTime, new TimingTargetAdapter {
      override def timingEvent(fraction:Float) = {
        currentAnimatorFraction = fraction
        def doHighlighting(t:JXTable, map:HashMap[(Int,Int),RefreshedCell]) {
          for (index <- map.keySet) {
            val cell = map(index)
            val currentFraction = fraction + cell.currentFraction
            if (currentFraction < 1.0f) {
              val c = new Color(87,206,255,math.round((1.0f-currentFraction) * 255))
              cell.currentColour = c
              t.repaint(t.getCellRect(index._2, index._1, false))
            }
          }
        }
        doHighlighting(fullTable, refreshMainMap)
      }
      override def end = {
        refreshMainMap.clear
        refreshRowMap.clear
        refreshColMap.clear
      }
    }).start
  }

  def updateRefreshHighlighter(row:Set[(Int,Int)], col:Set[(Int,Int)], main:Set[(Int,Int)]) {
    // This is being called once we have figured out what is different about this page compared to the last page.
    for (index <- row) {
      refreshRowMap(index) = RefreshedCell(index,0.0f)
    }
    for (index <- col) {
      refreshColMap(index) = RefreshedCell(index,0.0f)
    }
    for (index <- main) {
      refreshMainMap(index) = RefreshedCell(index,0.0f)
    }
    startAnimation
  }

  allTables foreach Highlighters.applyHighlighters
//  fullTable.addHighlighter(refreshMainHighlighter)

  model.setPivotChangedListener((pivotFieldsState)=> {
    publish(FieldsChangedEvent(pivotFieldsState))
  })

  val reportSpecificPanels = PivotTableViewHelper.generateReportSpecificPanels(data.pivotFieldsState,
    new ReportSpecificOptions(data.reportSpecificOptions), model)
  val reportPanelsAvailable = reportSpecificPanels.nonEmpty

  val iconToUse = if (otherLayoutInfo.hiddenType == AllHidden) StarlingIcons.im("/icons/16x15_out_fullscreen.png") else StarlingIcons.im("/icons/16x15_fullscreen.png")
  val fullScreenButton = new ImageButton(iconToUse, gotoFullScreen) {
    border = MatteBorder(1,1,0,1,BorderColour)
  }
  
  // I shouldn't be doing this here but if we don't have report specific panels, the filter field chooser should have a different border.
  if (reportPanelsAvailable) {
    filterComponent.border = MatteBorder(1,0,1,0,BorderColour)
  }

  private def gotoFullScreen {
    val (newHiddenType, newFrozen) = if (otherLayoutInfo.hiddenType == AllHidden) {
      val oht = otherLayoutInfo.oldHiddenType match {
        case Some(ht) => ht
        case None => NothingHidden
      }
      val of = otherLayoutInfo.oldFrozen match {
        case Some(f) => f
        case None => true
      }
      (oht, of)
    } else {
      (AllHidden, otherLayoutInfo.frozen)
    }
    publish(FullScreenSelectedEvent(otherLayoutInfo.hiddenType, newHiddenType, newFrozen))
  }

  val reportsPanel = new MigPanel("insets 0, gapx 0") {
    opaque = false
    for (rp <- reportSpecificPanels) {
      add(rp)
    }

    def scrolling() {}
  }
  val reportSpecificOptionsScrollPane = PivotTableViewHelper.generateScrollableFieldChooser(reportsPanel, reportsPanel, this)
  reportSpecificOptionsScrollPane.preferredSize = new Dimension(reportSpecificOptionsScrollPane.preferredSize.width, reportsPanel.preferredSize.height)
  reportSpecificOptionsScrollPane.minimumSize = new Dimension(10, reportsPanel.preferredSize.height)
  val rsScrollPaneHolder = new MigPanel("insets 1") {
    opaque = false
    add(reportSpecificOptionsScrollPane, "push,grow")
  }

  def pageResized(newSize:Dimension) {
    browserSize = newSize
    if (!otherLayoutInfo.frozen) {
      val extraWidth = if (otherLayoutInfo.hiddenType == FieldListHidden || otherLayoutInfo.hiddenType == AllHidden) hiddenFieldPanel.size.width else fieldPanel.size.width
      if (extraWidth + rowComponent.size.width + columnAndMeasureComponent.preferredSize.width > (browserSize.width - browserSize.width / 4)) {
        sizerPanel.preferredSize = new Dimension(10, sizerPanel.size.height)
      } else {
        sizerPanel.preferredSize = new Dimension(rowComponent.size.width-1, sizerPanel.size.height)
      }
      revalidate()
    }
  }

  def updateColumnAndMeasureScrollPane(updateSizerPanel:Boolean) {
    if (columnAndMeasureComponent != null) {
      columnAndMeasureScrollPane.preferredSize = new Dimension(columnAndMeasureScrollPane.preferredSize.width, columnAndMeasureComponent.preferredSize.height)
      columnAndMeasureScrollPane.minimumSize = new Dimension(10, columnAndMeasureComponent.preferredSize.height)

      if (!otherLayoutInfo.frozen && updateSizerPanel) {
        val extraWidth = if (otherLayoutInfo.hiddenType == FieldListHidden || otherLayoutInfo.hiddenType == AllHidden) hiddenFieldPanel.preferredSize.width else fieldPanel.preferredSize.width
        if (extraWidth + rowComponent.preferredSize.width + columnAndMeasureComponent.preferredSize.width > (browserSize.width - browserSize.width / 4)) {
          sizerPanel.preferredSize = new Dimension(10, sizerPanel.preferredSize.height)
        }
      }
      revalidate()
    }
  }
  updateColumnAndMeasureScrollPane(true)

  private val extraRow = if (reportPanelsAvailable) "[p]0" else ""

  def defaultComponentForFocus = if (otherLayoutInfo.frozen) {
    Some(rowHeaderTable)
  } else {
    Some(fullTable)
  }

  private val contentPanel = if (otherLayoutInfo.frozen) {
    new MigPanel("hidemode 2, insets 0", "[p]0[fill, grow]0[p]", extraRow + "[p]1[p]1[p]0[fill, grow]0[p]") {
      background = PivotTableBackgroundColour

      if (reportPanelsAvailable) {
        add(rsScrollPaneHolder, "spanx, growx, wrap")
        add(filterComponent, "spanx, split, growx, wrap")
      } else {
        add(filterComponent, "spanx, growx, wrap")
      }
      add(rowComponent, "spany 2, growx, ay bottom, ax left")
      add(columnAndMeasureScrollPane, "spanx, growx, wrap")
      add(columnHeaderScrollPanePanel, "skip 1, pushx, growx")

      add(fullScreenButton, "flowy, split, spany 2, gapbottom 0")
      add(mainVScrollBarHolder, "growy, wrap")
      
      add(rowHeaderTableScrollPanePanel, "push, grow")
      add(mainTableScrollPane, "wrap")
      add(mainHScrollBarHolder, "growx, span2")
    }
  } else {
    val gap = if (otherLayoutInfo.hiddenType == AllHidden) "0" else "1"
    new MigPanel("hidemode 2, insets 0", "[p]0[fill, grow]0[p]", extraRow + "[p]" + gap + "[p]" + gap + "[fill, grow]0[p]") {
      background = PivotTableBackgroundColour

      if (reportPanelsAvailable) {
        add(rsScrollPaneHolder, "spanx, growx, wrap")
        add(filterComponent, "spanx, split, growx, wrap")
      } else {
        add(filterComponent, "spanx, growx, wrap")
      }
      add(sizerPanel, "split, spanx, gapright 0")
      add(columnAndMeasureScrollPane, "growx, wrap")
      add(fullTableScrollPane, "push, grow, spanx 2")

      add(fullScreenButton, "flowy, split, gapbottom 0")
      add(fullVScrollBarHolder, "growy, wrap")

      add(fullHScrollBarHolder, "growx, spanx 2")
    }
  }

  if (otherLayoutInfo.hiddenType == FieldListHidden) {
    fieldChooserPanelHolder.setComponent(hiddenFieldPanel)
  } else {
    fieldChooserPanelHolder.setComponent(fieldPanel)
  }

  private val configTabbedPane = configPanels match {
    case None => None
    case Some(cp) => Some(new NTabbedPane(cp, true))
  }

  add(fieldChooserPanelHolder, "spany, growy")
  configPanels.foreach(cp => {
    add(configTabbedPane.get, "grow, wrap")
  })
  add(toolbarPanel, "growx, wrap")
  add(formulaBar, "growx, wrap")
  add(contentPanel, "push, grow")

  if (otherLayoutInfo.hiddenType == AllHidden) {
    fieldChooserPanelHolder.visible = false

    sizerPanel.visible = false

    configTabbedPane.foreach(_.visible = false)

    rsScrollPaneHolder.visible = false
    filterComponent.visible = false
    columnAndMeasureScrollPane.visible = false
    columnHeaderScrollPanePanel.visible = false

    toolbarPanel.visible = false
    formulaBar.visible = false
  }

  def resetDynamicState() {
    viewUI.resetImageProperties()
    fieldBeingDragged = false
    allDropTargets.foreach(_.reset())
  }

  def reverse() {
    tableModelsHelper.reverse(mainTable, rowHeaderTable, colHeaderTable)
    hideDropTargets()
    allDropTargets.foreach(_.reset())
    if (toolbarPanel.visible) reverseToolBarState()
    configPanels.foreach(_.revert())
    reportSpecificPanels.foreach(_.resetButton)
  }

  def selection = if (otherLayoutInfo.frozen) {
    (filtersWithSome, mainTable.getSelectedCells.map(tup => {
      val (row,col) = tup
      tableModelsHelper.mainTableModel.mapCellToFields(row, col)
    }))
  } else {
    (filtersWithSome, fullTable.getSelectedCells.map(tup => {
      val (row,col) = tup
      tableModelsHelper.fullTableModel.mapCellToFields(row, col)
    }))
  }

  configPanels match {
    case None =>
    case Some(cp) => {
      val action = cp.extraComponentAction
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), action.title)
      peer.getActionMap.put(action.title, action.peer)
    }
  }
}

case class RefreshedCell(index:(Int,Int), var currentFraction:Float) {
  var currentColour = new Color(0,0,255,128)
}
case class PivotTableViewRefreshState(refreshCellRowMap:HashMap[(Int,Int),RefreshedCell], refreshCellColMap:HashMap[(Int,Int),RefreshedCell], refreshCellMainMap:HashMap[(Int,Int),RefreshedCell]) extends ComponentRefreshState
case class AbstractPivotComponentOldPageData(rowData:Array[Array[AxisCell]], colData:Array[Array[AxisCell]], mainData:Array[Array[TableCell]]) extends OldPageData

class MapHighlighter(map:HashMap[(Int,Int),RefreshedCell]) extends UpdatingBackgroundColourHighlighter(new HighlightPredicate {
  def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
    map.keySet.contains((adapter.column,adapter.row))
  }
}, map)
