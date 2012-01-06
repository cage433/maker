package starling.pivot.view.swing

import fieldchoosers.{RowComponent, FilterComponent, FieldListComponent}
import scala.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import starling.pivot.model._
import starling.pivot._
import controller.{PivotTable, PivotTableConverter}
import starling.pivot.FieldChooserType._
import org.jdesktop.swingx.decorator.{HighlightPredicate}
import org.jdesktop.swingx.JXTable
import swing.event.{Event, MouseClicked, KeyPressed, KeyReleased}
import starling.rmi.PivotData
import scala.swing.Swing._
import starling.browser.common.GuiUtils._
import starling.gui.StarlingIcons
import java.awt.datatransfer.StringSelection
import java.awt.event._
import starling.gui.api.ReportSpecificOptions
import starling.utils.ImplicitConversions._
import collection.mutable.HashMap
import java.awt.{Graphics2D, Dimension, Color, GradientPaint, Cursor, AWTEvent, Toolkit, KeyboardFocusManager}
import starling.pivot.view.swing.PivotTableType._
import starling.pivot.HiddenType._
import starling.browser.common._
import starling.gui.pages.{PivotTablePageData, TableSelection, ConfigPanels}
import org.jdesktop.animation.timing.{TimingTargetAdapter, Animator}
import starling.browser.internal.StarlingBrowser
import starling.browser.{RefreshInfo, PreviousPageData, Modifiers}
import collection.immutable.{Map, List}
import starling.utils.Log
import starling.gui.custom.{EnterPressed, FindTextChanged, FindPanel}
import javax.swing._

object PivotTableView {
  def createWithLayer(data:PivotData, otherLayoutInfo:OtherLayoutInfo, browserSize:Dimension,
                      configPanel:(() => TableSelection)=>Option[ConfigPanels],
                      extraFormatInfo:ExtraFormatInfo, edits:PivotEdits, embedded:Boolean = true,
                      previousPageData:Option[PreviousPageData]) = {
    val viewUI = new PivotTableViewUI
    val view = new PivotTableView(data, otherLayoutInfo, browserSize, configPanel, extraFormatInfo, embedded, viewUI, edits, previousPageData)
    val layer = new SXLayerScala(view, viewUI)
    layer
  }
}

case class FieldsChangedEvent(pivotFieldState:PivotFieldsState) extends Event
case class TableDoubleClickEvent(filterFields:Seq[(Field,Selection)] , drillDownFields:Seq[(Field,Selection)], modifiers:Modifiers) extends Event
case class FullScreenSelectedEvent(currentState:HiddenType, newState:HiddenType, currentFrozen:Boolean) extends Event
case class ShowErrorsEvent(errors:Set[StackTrace]) extends Event
case class PivotEditsUpdatedEvent(edits:PivotEdits, source:PivotJTable) extends Event
case object SavePivotEdits extends Event
case class FieldPanelEvent(collapse:Boolean) extends Event
case class CollapsedStateUpdated(rowCollapsedState:Option[CollapsedState]=None, columnCollapsedState:Option[CollapsedState]=None) extends Event
case class GridSelectionEvent(selection:Option[(String,Boolean)]) extends Event

class PivotTableView(data:PivotData, otherLayoutInfo:OtherLayoutInfo, browserSize0:Dimension,
                     configPanel:(() => TableSelection)=>Option[ConfigPanels],
                     extraFormatInfo:ExtraFormatInfo, embedded:Boolean,
                     viewUI:PivotTableViewUI, edits:PivotEdits, previousPageData:Option[PreviousPageData])
        extends MigPanel("hidemode 2, insets 0, gap 0") {
  private var browserSize = browserSize0
  private val model = new starling.pivot.model.PivotTableModel(data)
  private var reverseToolBarState:()=>Unit = null

  var currentExtraFormatInfo = extraFormatInfo

  private val fieldListComponent = new FieldListComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable)
  private val columnAndMeasureComponent = new ColumnAndMeasureComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable)
  private val filterComponent = new FilterComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable)
  private val rowComponent = new RowComponent(model, otherLayoutInfo, viewUI, this, data.pivotTable)

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
      resizeSizerPanel()
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
      PivotTableView.this.revalidate()
      PivotTableView.this.repaint()

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

      override def paintComponent(g2:Graphics2D) {
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
        override def paintComponent(g2:Graphics2D) {
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
        override protected def paintBorder(g:Graphics2D) {
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
          fieldListComponent.reset()
          viewUI.resetImageProperties()
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

    override def paintComponent(g:Graphics2D) {
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
        def actionPerformed(e: ActionEvent) {PivotTableView.this.publish(FieldPanelEvent(false))}
      })
    }
    add(verticalButton)
  }

  def getColScrollPos = columnAndMeasureScrollPane.peer.getViewport.getViewPosition.x
  def setColScrollPos(pos:Int) {columnAndMeasureScrollPane.peer.getViewport.setViewPosition(new Point(pos,0))}
  def getRSScrollPos = reportSpecificOptionsScrollPane.peer.getViewport.getViewPosition.x
  def setRSScrollPos(pos:Int) {reportSpecificOptionsScrollPane.peer.getViewport.setViewPosition(new Point(pos,0))}
  def getMainScrollPos = if (otherLayoutInfo.frozen) {
    mainTableScrollPane.getViewport.getViewPosition
  } else {
    bottomTableScrollPane.getViewport.getViewPosition
  }
  def setMainScrollPos(pos:Point) {if (otherLayoutInfo.frozen) {
    mainTableScrollPane.getViewport.setViewPosition(pos)
  } else {
    bottomTableScrollPane.getViewport.setViewPosition(pos)
  }}
  def getSelectedCells = {
    if (otherLayoutInfo.frozen) {
      Right(mainTable.getSelectedCells, rowHeaderTable.getSelectedCells, colHeaderTable.getSelectedCells)
    } else {
      Left(topTable.getSelectedCells, bottomTable.getSelectedCells)
    }
  }

  def setSelectedCells(cells:Either[(List[(Int,Int)],List[(Int,Int)]), (List[(Int,Int)],List[(Int,Int)],List[(Int,Int)])]) {

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
      case Left((top,bottom)) => {
        topTable.setSelectedCells(top)
        bottomTable.setSelectedCells(bottom)
        if (bottom.nonEmpty) {
          if (bottom.size == 1) {
            val (r,c) = bottom.head
            doScrolling(bottomTable, bottomTableScrollPane, r, c)
          }
        }
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
      case Left((top, bottom)) => {
        if (top.nonEmpty) {
          val r = topTable.requestFocusInWindow()
          if (!r) {
            KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(topTable)
          }
        } else {
          val r = bottomTable.requestFocusInWindow()
          if (!r) {
            KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(bottomTable)
          }
        }
      }
      case Right((m,r,c)) => {
        if (m.nonEmpty) {
          mainTable.requestFocusInWindow()
        } else if (c.nonEmpty) {
          colHeaderTable.requestFocusInWindow()
        } else {
          rowHeaderTable.requestFocusInWindow()
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

  private val toolbarPanel = new MigPanel("insets 0") {
    visible = false
    if (embedded) {
      border = MatteBorder(1, 0, 0, 0, BorderColour)
    }
  }

  reactions += {
    case GridSelectionEvent(selection) => {
      val (text, summary) = selection.getOrElse(("", false))
      InfoBar.setText(text, summary)
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
    if (otherLayoutInfo.hiddenType != AllHidden) {
      rowComponent.border = BorderFactory.createMatteBorder(0,0,1,1,BorderColour)
    } else {
      rowComponent.border = BorderFactory.createMatteBorder(0,0,0,1,BorderColour)
    }
  }
  val sizerPanel = new FlowPanel {background = PivotTableBackgroundColour}

  private val previousPageData00:Option[(PivotTable, Map[(Int,Int),Float], Map[(Int,Int),Float], Map[(Int,Int),Float])] = previousPageData.map(ppd => {
    val pivotTable = ppd.pageData.asInstanceOf[PivotTablePageData].pivotData.pivotTable
    val refreshInfo = ppd.refreshInfo.get.asInstanceOf[PivotRefreshInfo]
    val currentFractionMap = refreshInfo.cells.map(c => {(c.row, c.column) -> c.currentFraction}).toMap
    val rowMap = refreshInfo.rowHeaderCells.map(c => ((c.row, c.column) -> c.currentFraction)).toMap
    val columnMap = refreshInfo.columnHeaderCells.map(c => ((c.row, c.column) -> c.currentFraction)).toMap
    (pivotTable, currentFractionMap, rowMap, columnMap)
  })
  private val previousPageData0 = previousPageData00.map(_._1)
  private val viewConverter = PivotTableConverter(otherLayoutInfo, data.pivotTable, extraFormatInfo, data.pivotFieldsState, previousPageData0)
  private val (rowHeaderData, colHeaderData, mainData, colUOMs, mainTableUpdateInfo, rowUpdateInfo, columnUpdateInfo) = Log.infoWithTime("Run pivot converter") {viewConverter.allTableCellsAndUOMs}

  private val mainOrBottomTableUpdateMap = new HashMap[(Int,Int),RefreshedCell]()
  private val rowHeaderTableUpdateMap = new HashMap[(Int,Int),RefreshedCell]()
  private val columnHeaderOrTopTableUpdateMap = new HashMap[(Int,Int),RefreshedCell]()
  val (addRows, addCols) = if (otherLayoutInfo.frozen) (0,0) else (0,rowHeaderData(0).length)
  mainTableUpdateInfo.foreach(c => {
    mainOrBottomTableUpdateMap += ((c.row + addRows, c.column + addCols) -> RefreshedCell(c.currentFraction))
  })
  previousPageData00.map(tup => {
    val mainMap = tup._2
    mainMap.foreach{case ((row, column), f) => {
      mainOrBottomTableUpdateMap.getOrElseUpdate((row + addRows, column + addCols), RefreshedCell(f))
    }}
  })
  if (otherLayoutInfo.frozen) {
    rowUpdateInfo.foreach(c => {
      rowHeaderTableUpdateMap += ((c.row, c.column) -> RefreshedCell(0.0f))
    })
    previousPageData00.map(tup => {
      val rowMap = tup._3
      rowMap.foreach{case (k, f) => {
        rowHeaderTableUpdateMap.getOrElseUpdate(k, RefreshedCell(f))
      }}
    })
    columnUpdateInfo.foreach(c => {
      columnHeaderOrTopTableUpdateMap += ((c.row, c.column) -> RefreshedCell(0.0f))
    })
    previousPageData00.map(tup => {
      val columnMap = tup._4
      columnMap.foreach{case (k, f) => {
        columnHeaderOrTopTableUpdateMap.getOrElseUpdate(k, RefreshedCell(f))
      }}
    })
  } else {
    rowUpdateInfo.foreach(c => {
      mainOrBottomTableUpdateMap += ((c.row + addRows, c.column) -> RefreshedCell(0.0f))
    })
    previousPageData00.map(tup => {
      val rowMap = tup._3
      rowMap.foreach{case ((row, column), f) => {
        val r = mainOrBottomTableUpdateMap.getOrElseUpdate((row + addRows, column), RefreshedCell(f))
        mainOrBottomTableUpdateMap += ((row + addRows, column) -> r)
      }}
    })
    columnUpdateInfo.foreach(c => {
      mainOrBottomTableUpdateMap += ((c.row, c.column + addCols) -> RefreshedCell(0.0f))
    })
    previousPageData00.map(tup => {
      val columnMap = tup._4
      columnMap.foreach{case ((row, column), f) => {
        val c = mainOrBottomTableUpdateMap.getOrElseUpdate((row, column + addCols), RefreshedCell(f))
        mainOrBottomTableUpdateMap += ((row, column + addCols) -> c)
      }}
    })
  }

  def refreshInfo = {
    val (mainRefreshInfo, rowRefreshInfo, columnRefreshInfo) = if (otherLayoutInfo.frozen) {
      val m = mainOrBottomTableUpdateMap.filter{case (_,v) => ((v.currentFraction + currentFraction) < 1.0f)}.map{case ((row, column), rc) => {PivotCellRefreshInfo(row, column, rc.currentFraction + currentFraction)}}.toList
      val r = rowHeaderTableUpdateMap.filter{case (_,v) => ((v.currentFraction + currentFraction) < 1.0f)}.map{case ((row,column), rc) => {PivotCellRefreshInfo(row, column, rc.currentFraction + currentFraction)}}.toList
      val c = columnHeaderOrTopTableUpdateMap.filter{case (_,v) => ((v.currentFraction + currentFraction) < 1.0f)}.map{case ((row,column), rc) => {PivotCellRefreshInfo(row, column, rc.currentFraction + currentFraction)}}.toList
      (m,r,c)
    } else {
      val (m0, rest) = mainOrBottomTableUpdateMap.filter{case (_, v) => ((v.currentFraction + currentFraction) < 1.0f)}.partition{case ((row, column), _) => {
        (row >= addRows) && (column >= addCols)
      }}

      val (r0, c0) = rest.partition{case ((row,_), _) => row >= addRows}
      val m = m0.map{case ((row, column), rc) => {PivotCellRefreshInfo(row - addRows, column - addCols, rc.currentFraction + currentFraction)}}.toList
      val r = r0.map{case ((row, column), rc) => {PivotCellRefreshInfo(row - addRows, column, rc.currentFraction + currentFraction)}}.toList
      val c = c0.map{case ((row, column), rc) => {PivotCellRefreshInfo(row, column - addCols, rc.currentFraction + currentFraction)}}.toList
      (m,r,c)
    }
    PivotRefreshInfo(mainRefreshInfo, rowRefreshInfo, columnRefreshInfo)
  }

  private def resizeColumnHeaderAndMainTableColumns() {
    tableModelsHelper.resizeColumnHeaderAndMainTableColumns(topTable, bottomTable, mainTable, colHeaderTable,
      columnHeaderScrollPane, columnHeaderScrollPanePanel, topTableScrollPane, topTableScrollPanePanel,
      mainTableScrollPane, otherLayoutInfo.columnDetails)
  }

  private def resizeRowHeaderTableColumns() {
    tableModelsHelper.resizeRowHeaderColumns(topTable, bottomTable, rowHeaderTable, rowComponent,
      data.pivotTable.rowFieldHeadingCount, sizerPanel, rowHeaderTableScrollPane, otherLayoutInfo.columnDetails)
  }

  private def updatePivotEdits(edits0:PivotEdits, tableType:PivotTableType) {
    val t = tableType match {
      case TopTable => topTable
      case BottomTable => bottomTable
      case RowHeader => rowHeaderTable
      case ColumnHeader => colHeaderTable
      case Main => mainTable
    }
    publish(PivotEditsUpdatedEvent(edits0, t))
  }

  private def updateNumberOfInPageEdits(numberOfInPageEdits:Int, validState:Boolean) {
    publish(NumberOfInPageEditsUpdated(numberOfInPageEdits, validState))
  }

  private def revalidateTheWholeThing() {
    onEDT({
      contentPanel.revalidate()
      contentPanel.repaint()
    })
  }

  private val tableModelsHelper = new PivotJTableModelHelper(mainData, data.pivotTable,
    rowHeaderData, colHeaderData, colUOMs, resizeColumnHeaderAndMainTableColumns(), resizeRowHeaderTableColumns(),
    data.pivotFieldsState, extraFormatInfo, edits, (edits0, tableType) => updatePivotEdits(edits0, tableType),
    (numberOfEdits, validState) => updateNumberOfInPageEdits(numberOfEdits, validState), revalidateTheWholeThing())

  def totalNumCols = tableModelsHelper.rowHeaderTableModel.getColumnCount + tableModelsHelper.mainTableModel.getColumnCount

  def allEdits = tableModelsHelper.allEdits(edits)

  def extraFormatInfoUpdated(extraFormatInfo:ExtraFormatInfo) {
    currentExtraFormatInfo = extraFormatInfo
    allDropTargets.foreach(_.extraFormatInfoUpdated())
    val newConverter = viewConverter.copy(extraFormatInfo = extraFormatInfo)
    val (newRowData,newColumnData,newMainTableCells,_,_,_,_) = newConverter.allTableCellsAndUOMs
    tableModelsHelper.setData(newRowData, newColumnData, newMainTableCells, extraFormatInfo)
    revalidateTheWholeThing()
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
  def copyReportToClip() {
    val convertSelectedCellsToString = if (lastTableToHaveFocus != null) {
      lastTableToHaveFocus.convertSelectedCellsToString
    } else {
      ""
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

  private val topTable = new PivotJTable(tableModelsHelper.topTableModel, this, model, indents, otherLayoutInfo.columnDetails, edits) {
    // I'm having to do this as page up and page down don't work for the full table as it is wrapped in a JXLayer - http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=7068740
    private var sortOutPageUpOrPageDown = 0
    override def processKeyBinding(ks:KeyStroke, e:KeyEvent, condition:Int, pressed:Boolean) = {
      if ((ks.getKeyCode == 33 || ks.getKeyCode == 34) && (condition == 1) && pressed) {
        sortOutPageUpOrPageDown = 2
      }
      super.processKeyBinding(ks, e, condition, pressed)
    }

    override def getParent = {
      if (sortOutPageUpOrPageDown > 0) {
        sortOutPageUpOrPageDown -= 1
        super.getParent.getParent
      } else {
        super.getParent
      }
    }
  }

  private def convertTopTableToString() = {
    val rows = Array.range(0, topTable.getRowCount)
    val cols = Array.range(0, topTable.getColumnCount())
    topTable.convertToString(rows, cols)
  }

  private val bottomTable = new PivotJTable(tableModelsHelper.bottomTableModel, this, model, indents,
    otherLayoutInfo.columnDetails, edits, Some(convertTopTableToString))
  private val mainTable = new PivotJTable(tableModelsHelper.mainTableModel, this, model, indents, otherLayoutInfo.columnDetails, edits)
  private val rowHeaderTable = new PivotJTable(tableModelsHelper.rowHeaderTableModel, this, model, indents, otherLayoutInfo.columnDetails, edits)
  private val colHeaderTable = new PivotJTable(tableModelsHelper.colHeaderTableModel, this, model, indents, otherLayoutInfo.columnDetails, edits)
  private val allTables = List(topTable, bottomTable, mainTable, rowHeaderTable, colHeaderTable)
  mainTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = mainTable}})
  colHeaderTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = colHeaderTable}})
  rowHeaderTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = rowHeaderTable}})
  topTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = topTable}})
  bottomTable.addFocusListener(new FocusAdapter {override def focusGained(e:FocusEvent) {lastTableToHaveFocus = bottomTable}})

  private var ignoreSelectionEvents = false

  class TableSelectionListener(table: PivotJTable, otherTablesToClearSelectionOn:PivotJTable*) extends ListSelectionListener {
    var lastEvent : Option[GridSelectionEvent] = None

    def valueChanged(e: ListSelectionEvent) {
      if (!ignoreSelectionEvents) { //skip events generated by clearing the selection on other tables
        ignoreSelectionEvents = true
        otherTablesToClearSelectionOn.foreach{ t => {
          t.getColumnModel.getSelectionModel.clearSelection()
          t.getSelectionModel.clearSelection()
        } }
        ignoreSelectionEvents = false
        val selection:Option[(String,Boolean)] = table.getSelectedCells match {
          case Nil => None
          case (row, col) :: Nil => {
            table.getValueAt(row, col) partialMatch {
              case tc:TableCell => (tc.longText.getOrElse(tc.text), false)
              case ac:AxisCell => (ac.duplicateText.getOrElse(ac.longLabel), false)
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
              val max = values.max
              val min = values.min
              val sum = values.sum
              val count = values.size
              val average = sum / count
              Some(("Max/Min: " + max + "/" + min + "     Avg: " + average + "     Count: " + count + "     Sum: " + sum, true))
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
    listenForSelection(topTable, bottomTable)
    listenForSelection(bottomTable, topTable)
    listenForSelection(mainTable, rowHeaderTable, colHeaderTable)
    listenForSelection(rowHeaderTable, mainTable, colHeaderTable)
    listenForSelection(colHeaderTable, rowHeaderTable, mainTable)
  }

  def giveDefaultFocus() {
    if (otherLayoutInfo.frozen) {
      mainTable.requestFocusInWindow
    } else {
      bottomTable.requestFocusInWindow
    }
  }

  private val InfoBar = new MigPanel("insets 0, gap 0") {
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
        revalidate()
        repaint()
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
      multilineText.text = t
      if (summary) {
        onelineText.foreground = GuiUtils.BlueTextColour
        onelineText.peer.setHorizontalAlignment(SwingConstants.RIGHT)
        if (expanded) shrink()
        expandButtonHolder.visible = false
      } else {
        onelineText.foreground = Color.BLACK
        onelineText.peer.setHorizontalAlignment(SwingConstants.LEFT)
        expandButtonHolder.visible = true
      }
    }

    def expand() {
      expanded = true
      expandButtonHolder.expandButton.icon = StarlingIcons.icon("/icons/scroll_up.png")
      textHolder.update(multilineText)
    }

    def shrink() {
      expanded = false
      expandButtonHolder.expandButton.icon = StarlingIcons.icon("/icons/scroll_down.png")
      textHolder.update(onelineText)
    }

    val expandButtonHolder = new MigPanel("insets 3lp 0 0 0") {
      border = MatteBorder(1,0,1,1,BorderColour)
      val expandButton = new Label {
        icon = StarlingIcons.icon("/icons/scroll_down.png")
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
        reactions += {
          case scala.swing.event.MousePressed(_,_,_,_,_) => {
            if (expanded) {
              shrink()
            } else {
              expand()
            }
          }
        }
        listenTo(mouse.clicks)
      }
      add(expandButton, "push, ay top")
    }

    val infoPanel = new MigPanel("insets 2 0 0 0") {
      border = MatteBorder(1,0,1,0,BorderColour)
      def l(t:String) = new Label(t) {foreground = GuiUtils.BlueTextColour}
      val rowAndColumnNumberLabel = l(" [" + mainTable.getRowCount.toString + " x " + mainTable.getColumnCount().toString + "] ")

      add(rowAndColumnNumberLabel, "ay top")
    }

    shrink()

    add(iconHolder, "grow")
    add(textHolder, "push,grow")
    add(expandButtonHolder, "grow, hidemode 3")
    add(infoPanel, "grow")
  }

  private val (bottomTableScrollPane, bottomHScrollBarHolder, bottomVScrollBarHolder) = PivotTableViewHelper.generateScrollPaneHolders(bottomTable)
  bottomTableScrollPane.setBorder(BorderFactory.createMatteBorder(1,0,0,0,BorderColour))

  private val topTableScrollPane = {
    val tableLayerUI = new UnfrozenTableLayerUI(rowComponent)
    val layer = new SXLayer(topTable, tableLayerUI) {
      setLayerEventMask(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK
        //| AWTEvent.MOUSE_WHEEL_EVENT_MASK - mouse wheel event's will pass through
        | AWTEvent.KEY_EVENT_MASK | AWTEvent.FOCUS_EVENT_MASK)
    }.peer
    new JScrollPane(layer)
  }
  topTableScrollPane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0))
  topTableScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
  topTableScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)

  private val topTableScrollPanePanel = new MigPanel("insets 0", "", "[grow, bottom]") {
    add(topTableScrollPane, "pushx,growx")
    if (otherLayoutInfo.hiddenType != AllHidden) {
      border = BorderFactory.createMatteBorder(1, 0, 0, 0, BorderColour)
    }
  }

  private val topTableHorizontalScrollBar = topTableScrollPane.getHorizontalScrollBar
  topTableHorizontalScrollBar.setPreferredSize(new Dimension(0, 0))
  topTableHorizontalScrollBar.setModel(bottomTableScrollPane.getHorizontalScrollBar.getModel)

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

  tableModelsHelper.resizeRowHeaderColumns(topTable, bottomTable, rowHeaderTable, rowComponent,
    data.pivotTable.rowFieldHeadingCount, sizerPanel, rowHeaderTableScrollPane, otherLayoutInfo.columnDetails)
  tableModelsHelper.resizeColumnHeaderAndMainTableColumns(topTable, bottomTable, mainTable, colHeaderTable, columnHeaderScrollPane,
    columnHeaderScrollPanePanel, topTableScrollPane, topTableScrollPanePanel, mainTableScrollPane, otherLayoutInfo.columnDetails)

  allTables foreach Highlighters.applyHighlighters
  if (otherLayoutInfo.frozen) {
    mainTable.addHighlighter(new MapHighlighter(mainOrBottomTableUpdateMap))
    rowHeaderTable.addHighlighter(new MapHighlighter(rowHeaderTableUpdateMap))
  } else {
    bottomTable.addHighlighter(new MapHighlighter(mainOrBottomTableUpdateMap))
  }

  private var currentFraction = 0.0f

  private def startAnimation() {
    val tablesAndUpdateMaps = if (otherLayoutInfo.frozen) {
      List((mainTable, mainOrBottomTableUpdateMap), (rowHeaderTable, rowHeaderTableUpdateMap), (colHeaderTable, columnHeaderOrTopTableUpdateMap))
    } else {
      List((bottomTable, mainOrBottomTableUpdateMap))
    }
    new Animator(StarlingBrowser.RefreshTime, new TimingTargetAdapter {
      override def timingEvent(fraction:Float) {
        currentFraction = fraction
        def doHighlighting(t:JXTable, map:HashMap[(Int,Int),RefreshedCell]) {
          for (index <- map.keySet) {
            val cell = map(index)
            val currentFraction = fraction + cell.currentFraction
            if (currentFraction < 1.0f) {
              val c = new Color(255,255,0, math.round((1.0f - currentFraction) * 128))
              cell.currentColour = c
              t.repaint(t.getCellRect(index._1, index._2, false))
            }
          }
        }
        tablesAndUpdateMaps.foreach{case (t,m) => doHighlighting(t, m)}
      }
      override def end() {
        mainOrBottomTableUpdateMap.clear()
        rowHeaderTableUpdateMap.clear()
        columnHeaderOrTopTableUpdateMap.clear()
      }
    }).start()
  }

  model.setPivotChangedListener((pivotFieldsState)=> {
    publish(FieldsChangedEvent(pivotFieldsState))
  })

  val reportSpecificPanels = PivotTableViewHelper.generateReportSpecificPanels(data.pivotFieldsState,
    new ReportSpecificOptions(data.reportSpecificOptions), model)
  val reportPanelsAvailable = reportSpecificPanels.nonEmpty

  val iconToUse = if (otherLayoutInfo.hiddenType == AllHidden) StarlingIcons.im("/icons/16x15_out_fullscreen.png") else StarlingIcons.im("/icons/16x15_fullscreen.png")
  val fullScreenButton = new ImageButton(iconToUse, gotoFullScreen()) {
    if (otherLayoutInfo.hiddenType == HiddenType.AllHidden) {
      border = MatteBorder(0,1,0,1,BorderColour)
    } else {
      border = MatteBorder(1,1,0,1,BorderColour)
    }
  }
  
  // I shouldn't be doing this here but if we don't have report specific panels, the filter field chooser should have a different border.
  if (reportPanelsAvailable) {
    filterComponent.border = MatteBorder(1,0,1,0,BorderColour)
  }

  private def gotoFullScreen() {
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
    resizeSizerPanel()
  }

  private def resizeSizerPanel() {
    if (!otherLayoutInfo.frozen) {
      val extraWidth = if (otherLayoutInfo.hiddenType == FieldListHidden || otherLayoutInfo.hiddenType == AllHidden) hiddenFieldPanel.size.width else fieldPanel.size.width
      val totalSize = extraWidth + rowComponent.preferredSize.width + columnAndMeasureComponent.preferredSize.width
      if (totalSize >= browserSize.width) {
        val delta = totalSize - browserSize.width
        val widthToUse = math.max(10, rowComponent.preferredSize.width-1 - delta)
        val d = new Dimension(widthToUse, sizerPanel.size.height)
        sizerPanel.preferredSize = d
        sizerPanel.minimumSize = d
      } else {
        val d = new Dimension(rowComponent.preferredSize.width-1, sizerPanel.size.height)
        sizerPanel.preferredSize = d
        sizerPanel.minimumSize = d
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
        val totalSize = extraWidth + rowComponent.preferredSize.width + columnAndMeasureComponent.preferredSize.width
        if (totalSize >= browserSize.width) {
          val delta = totalSize - browserSize.width
          val widthToUse = math.max(10, rowComponent.preferredSize.width-1 - delta)
          val d = new Dimension(widthToUse, sizerPanel.preferredSize.height)
          sizerPanel.preferredSize = d
          sizerPanel.minimumSize = d
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
    Some(bottomTable)
  }

  private val bottomRightCornerPanel = new MigPanel("insets 0") {
    background = Color.WHITE
    border = MatteBorder(0,0,1,1,BorderColour)
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
      add(mainHScrollBarHolder, "growx, spanx 2")
      add(bottomRightCornerPanel, "grow")
    }
  } else {
    val gap = if (otherLayoutInfo.hiddenType == AllHidden) "0" else "1"
    new MigPanel("hidemode 2, insets 0", "[p]0[fill, grow]0[p]", extraRow + "[p]" + gap + "[p]" + gap + "[p]0[fill, grow]0[p]") {
      background = PivotTableBackgroundColour

      if (reportPanelsAvailable) {
        add(rsScrollPaneHolder, "spanx, growx, wrap")
        add(filterComponent, "spanx, split, growx, wrap")
      } else {
        add(filterComponent, "spanx, growx, wrap")
      }
      add(sizerPanel, "split, spanx, gapright 0")
      add(columnAndMeasureScrollPane, "growx, wrap")

      add(topTableScrollPanePanel, "pushx, growx, spanx 2")

      add(fullScreenButton, "flowy, split, spany 2, gapbottom 0")
      add(bottomVScrollBarHolder, "growy, wrap")

      add(bottomTableScrollPane, "push, grow, spanx 2, wrap")

      add(bottomHScrollBarHolder, "growx, spanx 2")
      add(bottomRightCornerPanel, "grow")
    }
  }

  if (otherLayoutInfo.hiddenType == FieldListHidden) {
    fieldChooserPanelHolder.setComponent(hiddenFieldPanel)
  } else {
    fieldChooserPanelHolder.setComponent(fieldPanel)
  }

  val actualConfigPanel = configPanel(selection _)

  private val configTabbedPane = actualConfigPanel match {
    case None => None
    case Some(cp) => Some(new NTabbedPane(cp, true))
  }

  private def findText(text:String, direction:Int, moveBeforeFind:Boolean) {
    val rowCount = topTable.getRowCount + bottomTable.getRowCount
    val colCount = bottomTable.getColumnCount()
    val selectedCells = getSelectedCells
    
    var (currentRow, currentCol) = selectedCells match {
      case Left((topCells, bottomCells)) => {
        if (bottomCells.nonEmpty) {
          val (row, col) = bottomCells.last
          (row + topTable.getRowCount, col)
        } else if (topCells.nonEmpty) {
          topCells.last
        } else {
          (0,0)
        }
      }
      case Right((mainCells, rowCells, columnCells)) => {
        if (mainCells.nonEmpty) {
          val (row, col) = mainCells.last
          (row + colHeaderTable.getRowCount, col + rowHeaderTable.getColumnCount())
        } else if (columnCells.nonEmpty) {
          val (row, col) = columnCells.last
          (row, col + rowHeaderTable.getColumnCount())
        } else if (rowCells.nonEmpty) {
          val (row, col) = rowCells.last
          (row + colHeaderTable.getRowCount, col)
        } else {
          (0,0)
        }
      }
    }

    def updatePosition() {
      def sortOutRow() {
        currentRow += direction
        if (currentRow == rowCount) {
          currentRow = 0
        } else if (currentRow == -1) {
          currentRow = rowCount - 1
        }
      }

      currentCol += direction
      if (currentCol == colCount) {
        currentCol = 0
        sortOutRow()
      } else if (currentCol == -1) {
        currentCol = colCount - 1
        sortOutRow()
      }
    }

    if (moveBeforeFind) {
      updatePosition()
    }

    val startRow = currentRow
    val startCol = currentCol
    var found = false
    var notFound = false
    while (!found && !notFound) {
      val searchValue = {
        val (tableToUse, rowToUse) = if (currentRow < topTable.getRowCount) (topTable, currentRow) else (bottomTable, currentRow -topTable.getRowCount)
        (tableToUse.getValueAt(rowToUse, currentCol) match {
          case tc:TableCell => tc.text
          case ac:AxisCell => ac.text
        })
      }.toLowerCase
      if (searchValue.contains(text)) {
        found = true
      } else {
        updatePosition()
        if ((currentRow == startRow) && (currentCol == startCol)) {
          notFound = true
        }
      }
    }

    if (found) {
      val (rowToUse, columnToUse, tableToUse) = selectedCells match {
        case Left(_) => {
          if (currentRow < topTable.getRowCount) {
            (currentRow, currentCol, topTable)
          } else {
            (currentRow - topTable.getRowCount, currentCol, bottomTable)
          }
        }
        case Right(_) => {
          if (currentRow >= colHeaderTable.getRowCount && currentCol >= rowHeaderTable.getColumnCount()) {
            (currentRow - colHeaderTable.getRowCount, currentCol - rowHeaderTable.getColumnCount(), mainTable)
          } else if (currentRow >= colHeaderTable.getRowCount) {
            (currentRow - colHeaderTable.getRowCount, currentCol, rowHeaderTable)
          } else {
            (currentRow, currentCol - rowHeaderTable.getColumnCount(), colHeaderTable)
          }
        }
      }
      tableToUse.setRowSelectionInterval(rowToUse, rowToUse)
      tableToUse.setColumnSelectionInterval(columnToUse, columnToUse)
      tableToUse.scrollRectToVisible(new Rectangle(tableToUse.getCellRect(rowToUse, columnToUse, true)))
    }
  }

  val findBar = new MigPanel("insets 3lp", "[p][p][p]3lp[p]") {
    visible = false
    border = MatteBorder(0,0,1,1,BorderColour)

    def hidePanel() {
      findPanel.resetText()
      previousButton.enabled = false
      nextButton.enabled = false
      visible = false
      updateFocusBasedOnCellSelection()
    }

    val hidePanelAction = swing.Action("hidePanelAction") {hidePanel()}
    peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "hidePanelAction")
    peer.getActionMap.put("hidePanelAction", hidePanelAction.peer)

    val closeButton = new TwoFixedImagePanel(
      StarlingIcons.im("/icons/close.png"),
      StarlingIcons.im("/icons/stop.png"),
      hidePanel()) {
      tooltip = "Close find bar"
      focusable = false
    }
    val previousButton = new Button {
      text = "Previous"
      mnemonic = swing.event.Key.P
      icon = StarlingIcons.icon("/icons/16x16_previous.png")
      focusable = false
      enabled = false
      reactions += {
        case swing.event.ButtonClicked(e) => findText(findPanel.findText.toLowerCase, -1, true)
      }
    }
    val nextButton = new Button {
      text = "Next"
      mnemonic = swing.event.Key.N
      icon = StarlingIcons.icon("/icons/16x16_next.png")
      focusable = false
      enabled = false
      reactions += {
        case  swing.event.ButtonClicked(e) => findText(findPanel.findText.toLowerCase, 1, true)
      }
    }

    val findPanel = new FindPanel

    reactions += {
      case EnterPressed(false) => findText(findPanel.findText.toLowerCase, 1, true)
      case EnterPressed(true) => findText(findPanel.findText.toLowerCase, -1, true)
      case FindTextChanged(text) => {
        if (text.nonEmpty) {
          nextButton.enabled = true
          previousButton.enabled = true
          findText(text.toLowerCase, 1, false)
        } else {
          nextButton.enabled = false
          previousButton.enabled = false
        }
      }
    }
    listenTo(findPanel)

    add(closeButton)
    add(findPanel, "growy")
    add(previousButton)
    add(nextButton)
  }

  add(fieldChooserPanelHolder, "spany, growy")
  actualConfigPanel.foreach(cp => {
    add(configTabbedPane.get, "grow, wrap")
  })
  add(toolbarPanel, "growx, wrap")
  add(InfoBar, "growx, wrap")
  add(contentPanel, "push, grow, wrap")
  add(findBar, "growx")

  if (otherLayoutInfo.hiddenType == AllHidden) {
    fieldChooserPanelHolder.visible = false

    sizerPanel.visible = false

    configTabbedPane.foreach(_.visible = false)

    rsScrollPaneHolder.visible = false
    filterComponent.visible = false
    columnAndMeasureScrollPane.visible = false
    columnHeaderScrollPanePanel.visible = false

    toolbarPanel.visible = false
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
    actualConfigPanel.foreach(_.revert())
    reportSpecificPanels.foreach(_.resetButton)
    resizeSizerPanel()
  }

  def selection = {
    val r = if (otherLayoutInfo.frozen) {
      (filtersWithSome, mainTable.getSelectedCells.map(tup => {
        val (row,col) = tup
        tableModelsHelper.mainTableModel.mapCellToFieldsForMainTable(row, col)
      }))
    } else {
      (filtersWithSome, bottomTable.getSelectedCells.map(tup => {
        val (row,col) = tup
        tableModelsHelper.bottomTableModel.mapCellToFieldsForMainTable(row, col)
      }))
    }
    TableSelection(r)
  }
  private val showFindAction = swing.Action("showFindAction") {
    findBar.visible = true
    findBar.findPanel.findFieldRequestFocus()
  }
  allTables.foreach(t => {
    t.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, InputEvent.CTRL_DOWN_MASK), "showFindAction")
    t.getActionMap.put("showFindAction", showFindAction.peer)
  })

  actualConfigPanel match {
    case None =>
    case Some(cp) => {
      val action = cp.extraComponentAction
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), action.title)
      peer.getActionMap.put(action.title, action.peer)
    }
  }

  if (mainOrBottomTableUpdateMap.nonEmpty || rowHeaderTableUpdateMap.nonEmpty || columnHeaderOrTopTableUpdateMap.nonEmpty) {
    startAnimation()
  }
}

case class RefreshedCell(var currentFraction:Float) {
  var currentColour = new Color(255,255,0,128)
}

class MapHighlighter(map:HashMap[(Int,Int),RefreshedCell]) extends UpdatingBackgroundColourHighlighter(new HighlightPredicate {
  def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
    map.keySet.contains((adapter.row,adapter.column))
  }
}, map)

case class PivotCellRefreshInfo(row:Int, column:Int, currentFraction:Float)
case class PivotRefreshInfo(cells:List[PivotCellRefreshInfo], rowHeaderCells:List[PivotCellRefreshInfo],
                            columnHeaderCells:List[PivotCellRefreshInfo]) extends RefreshInfo