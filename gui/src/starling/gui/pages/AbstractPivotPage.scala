package starling.gui.pages

import scala.swing._
import event.ButtonClicked
import starling.gui._
import starling.pivot.view.swing._
import starling.pivot._
import controller.{PivotTable, PivotTableConverter}
import starling.rmi.PivotData
import starling.utils.ImplicitConversions._
import scala.swing.Swing._
import java.awt.{Color, Dimension}
import starling.pivot.HiddenType._
import starling.browser._
import common.{GuiUtils, MigPanel}
import starling.reports.facility.ReportFacility

/**
 * An abstract page which holds a pivot table
 * The subclasses define:
 *  -how to get the PivotTableDataSource
 *  -the custom component
 *  -the title
 *  -what page to goto on drilldown (after the groups have been exausted)
 *
 */

abstract class AbstractStarlingPivotPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null) extends
  AbstractPivotPage(pivotPageState, edits) with StarlingServerPage {
}

case class TableSelection(selection:(List[(Field, Selection)], List[scala.List[(Field, Selection)]]))

abstract class AbstractPivotPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null) extends Page {
  def icon = StarlingIcons.im("/icons/stock_chart-reorganize.png")
  def build(sc: SC) = PivotTablePageData(dataRequest(sc), subClassesPageData(sc))
  def dataRequest(pageBuildingContext:SC):PivotData
  def save(sc:ServerContext, edits:PivotEdits):Int = throw new Exception("No implementation of save for this page")
  def postSave(i:Int, context:PageContext) {throw new Exception("No implementation of postSave for this page")}
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null):Page
  def subClassesPageData(pageBuildingContext:SC):Option[PageData] = None
  def finalDrillDownPage(fields:Seq[(Field,Selection)], pageContext:PageContext, modifiers:Modifiers) {}
  def toolbarButtons(pageContext: PageContext, data:PageData):List[Button] = List()
  def configPanel(pageContext:PageContext, data:PageData, tableSelection:() => TableSelection):Option[ConfigPanels] = None
  def createComponent(pageContext:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) : PageComponent = {
    PivotComponent(text, pageContext, toolbarButtons(pageContext, data), configPanel, finalDrillDownPage, selfPage,
      data, pivotPageState, edits, save, postSave, bookmark, browserSize, false, previousPageData)
  }
}

trait Revertable {
  def revert()
}

trait ConfigPanel extends Component with Revertable {
  def displayName:String
  def revert() {this match {
    case container: Container => container.contents.filterCast[Revertable].foreach(_.revert())
    case _ => throw new Exception("No implemented")
  }}
}

case class ConfigPanels(configPanels:List[ConfigPanel], extraComponent:Component, extraComponentAction:Action) extends Revertable {
  def revert() {configPanels.foreach(_.revert())}
}

case class PivotPageState(showChart:Boolean=false, pivotFieldParams:PivotFieldParams=PivotFieldParams(true, None),
                          otherLayoutInfo:OtherLayoutInfo = OtherLayoutInfo(totals = Totals.Null)) {
  def copyPivotFieldsState(pivotFieldsState : PivotFieldsState) =
    copy(pivotFieldParams = pivotFieldParams.copy(pivotFieldState = Some(pivotFieldsState)))

  def copyPivotFieldsState(pivotFieldsState:Option[PivotFieldsState]) =
    copy(pivotFieldParams = pivotFieldParams.copy(pivotFieldState = pivotFieldsState))

  def copyLayout(pivotLayout: PivotLayout) =
    copy(pivotFieldParams = pivotFieldParams.copy(pivotFieldState = Some(pivotLayout.pivotFieldState)),
         otherLayoutInfo = pivotLayout.otherLayoutInfo)

  def copyTotals(totalsF : Totals => Totals) = copy(otherLayoutInfo = otherLayoutInfo.copy(totals = totalsF(otherLayoutInfo.totals)))

  def flipCalculate =
    copy(pivotFieldParams = pivotFieldParams.copy(calculate = !pivotFieldParams.calculate))
}
object PivotPageState {
  def default(pivotFieldsState:PivotFieldsState) = {
    PivotPageState(pivotFieldParams = PivotFieldParams(true, Some(pivotFieldsState)))
  }
}
case class PivotTablePageData(pivotData:PivotData,subClassesPageData:Option[PageData]) extends PageData

object PivotComponent {
  def apply(text:String,
        pageContext:PageContext,
        toolbarButtons:List[Button],
        configPanel:(PageContext, PageData, () => TableSelection)=>Option[ConfigPanels],
        finalDrillDown:(Seq[(Field,Selection)],PageContext,Modifiers)=>Unit,
        selfPage:((PivotPageState,PivotEdits)=>Page),
        pageData:PageData,
        pivotPageState:PivotPageState,
        edits:PivotEdits,
        save:(ServerContext, PivotEdits) => Int,
        postSave:(Int, PageContext) => Unit,
        bookmark:Bookmark,
        browserSize:Dimension,
        embedded:Boolean = true,
        previousPageData:Option[PreviousPageData]):PivotComponent = {

    val data = pageData.asInstanceOf[PivotTablePageData]

    if (pivotPageState.showChart) {
      new PivotTablePageGraphComponent(data.pivotData.pivotTable)
    } else {
      new PivotTablePageComponent(text, pageContext, toolbarButtons, configPanel, finalDrillDown, selfPage, data,
        pivotPageState, edits, save, postSave, bookmark, browserSize, embedded, previousPageData)
    }
  }
}

abstract class PivotComponent extends MigPanel("insets 0", "[fill,grow]", "[fill,grow]") with PageComponent {
  def getSelection:TableSelection
}

class PivotTablePageGraphComponent(table:PivotTable) extends PivotComponent {
  val pivotGrid = new PivotTableConverter(OtherLayoutInfo(), table).createGrid(addExtraColumnRow = false)
  val pivotChart = new PivotChartView(pivotGrid)
  add(pivotChart, "push, grow")

  def getSelection = TableSelection((List(), List(List())))
}

class PivotTablePageComponent(
        text:String,
        pageContext:PageContext,
        toolbarButtons:List[Button],
        configPanel:(PageContext, PageData, () => TableSelection)=>Option[ConfigPanels],
        finalDrillDown:(Seq[(Field,Selection)],PageContext,Modifiers)=>Unit,
        selfPage:((PivotPageState,PivotEdits)=>Page),
        pivotTablePageData:PivotTablePageData,
        pivotPageState:PivotPageState,
        edits:PivotEdits,
        save:(ServerContext, PivotEdits) => Int,
        postSave:(Int, PageContext) => Unit,
        bookmark:Bookmark,
        browserSize:Dimension,
        embedded:Boolean,
        previousPageData:Option[PreviousPageData]) extends PivotComponent {

  val data = pivotTablePageData.pivotData
  val extraFormatInfo = pageContext.getSetting(StandardUserSettingKeys.ExtraFormattingInfo, PivotFormatter.DefaultExtraFormatInfo)
  val pivotTableComponent = PivotTableView.createWithLayer(data, pivotPageState.otherLayoutInfo, browserSize,
    (selectionF) => { configPanel(pageContext,pivotTablePageData,selectionF)}, extraFormatInfo, edits, embedded, previousPageData)
  val pivotComp = pivotTableComponent.getScalaComponent

  val currentFieldState = data.pivotFieldsState
  val drillDownGroups = data.drillDownGroups

  val toolBar = new MigPanel("insets 1 1 1 1, gap 1") {
    val lockScreenButton = new ToggleToolBarButton {
      icon = StarlingIcons.Lock
      tooltip = "Freeze or unfreeze the column headers"
      selected = pivotPageState.otherLayoutInfo.frozen
      reactions += {case ButtonClicked(b) => {
        pageContext.goTo(selfPage(pivotPageState.copy(
          otherLayoutInfo = pivotPageState.otherLayoutInfo.copy(frozen = !pivotPageState.otherLayoutInfo.frozen)), edits))
      }}
    }

    val expandColumnsToFitButton = new ToggleToolBarButton {
      icon = StarlingIcons.ExpandColumnsToFit
      tooltip = "Expand columns to fit or default column sizes"
      selected = pivotPageState.otherLayoutInfo.columnDetails.expandToFit
      reactions += {
        case ButtonClicked(b) => {
          val newColumnDetails = pivotPageState.otherLayoutInfo.columnDetails.copy(expandToFit = !pivotPageState.otherLayoutInfo.columnDetails.expandToFit)
          pageContext.goTo(selfPage(pivotPageState.copy(
            otherLayoutInfo = pivotPageState.otherLayoutInfo.copy(columnDetails = newColumnDetails)), edits))
        }
      }
    }

    val rotateButton = new ToolBarButton {
      icon = StarlingIcons.Rotate
      tooltip = "Switch the row and column fields"
      reactions += {
        case ButtonClicked(_) => { pageContext.goTo(selfPage(pivotPageState.copyPivotFieldsState(currentFieldState.rotate), edits)) }
      }
      enabled = currentFieldState.hasRowOrColumnFields
    }

    val bottomTotalsButton = new ToggleToolBarButton {
      icon = StarlingIcons.RowGrandTotals
      tooltip = "Display or hide the row grand totals"
      reactions += { case ButtonClicked(_) => {pageContext.goTo(selfPage(pivotPageState.copyTotals(_.toggleRowGrandTotal), edits))} }
    }
    val rightTotalsButton = new ToggleToolBarButton {
      icon = StarlingIcons.ColumnTotals
      tooltip = "Display or hide the column grand totals"
      reactions += { case ButtonClicked(_) => {pageContext.goTo(selfPage(pivotPageState.copyTotals(_.toggleColumnGrandTotal), edits))} }
    }
    val rowSubTotalsButton = new ToggleToolBarButton {
      icon = StarlingIcons.RowSubTotals
      tooltip = "Display or hide the row sub totals"
      reactions += { case ButtonClicked(_) => {
        val rowFields = currentFieldState.rowFields
        val newDisabledSubTotals = pivotPageState.otherLayoutInfo.disabledSubTotals.filterNot(rowFields.contains(_))
        val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(disabledSubTotals = newDisabledSubTotals)
        val newPivotPageState = pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo)
        pageContext.goTo(selfPage(newPivotPageState.copyTotals(_.toggleRowSubTotals), edits))
      } }
    }
    val columnSubTotalsButton = new ToggleToolBarButton {
      icon = StarlingIcons.ColumnSubTotals
      tooltip = "Display or hide the column sub totals"
      reactions += { case ButtonClicked(_) => {
        val colFields = currentFieldState.columns.allFields
        val newDisabledSubTotals = pivotPageState.otherLayoutInfo.disabledSubTotals.filterNot(colFields.contains(_))
        val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(disabledSubTotals = newDisabledSubTotals)
        val newPivotPageState = pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo)
        pageContext.goTo(selfPage(newPivotPageState.copyTotals(_.toggleColumnSubTotals), edits))
      } }
    }
    val chartButton = new ToolBarButton {
      icon = StarlingIcons.Chart
      tooltip = "Display a chart of table data"
      reactions += { case ButtonClicked(_) => { pageContext.goTo(selfPage(pivotPageState.copy(showChart = true), edits)) } }
    }
    val clearCacheButton = new ToolBarButton {
      icon = StarlingIcons.icon("/icons/16x16_edit-clear.png")
      tooltip = "Clear local and server side caches - this could cause major problems and is only available in dev"
      reactions += {
        case ButtonClicked(b) => {
          // Clear the local cache.
          pageContext.clearCache()
          pageContext.submit(ClearServerSideCache)
        }
      }
    }
    val toggleCalculateButton = new ToggleToolBarButton {
      selected = !pivotPageState.pivotFieldParams.calculate
      tooltip = "Enable or disable calculation on the fly"
      icon = StarlingIcons.Calculate
      // If the button is deselected it means we need to calculate with the new layout.
      reactions += {
        case ButtonClicked(_) => {
          pageContext.goTo(selfPage(pivotPageState.flipCalculate, edits))
        }
      }
    }
    val removeZerosButton = new ToggleToolBarButton {
      selected = pivotPageState.otherLayoutInfo.removeZeros
      tooltip = "Remove or show rows that contain zeros"
      icon = StarlingIcons.icon("/icons/16x16_remove_zeros.png")
      reactions += {
        case ButtonClicked(_) => {
          val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(removeZeros = !pivotPageState.otherLayoutInfo.removeZeros)
          pageContext.goTo(selfPage(pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo), edits))
        }
      }
    }
    val copyButton = new ToolBarButton {
      icon = StarlingIcons.Copy
      tooltip = "Copy the selected cells to the clipboard"
      reactions += {case ButtonClicked(b) => pivotComp.copyReportToClip()}
    }
    val saveEditsButton = new ToolBarButton {
      icon = StarlingIcons.icon("/icons/16x16_save.png")
      tooltip = "Save the edits made to the data"
      enabled = edits.nonEmpty
      val numEdits = edits.size
      val numEditsString = numEdits.toString

      def saveEdits() {
        pageContext.submit(new SubmitRequest[Int] {
          def baseSubmit(serverContext:ServerContext) = {
            save(serverContext, edits)
          }
        }, onComplete = (r:Int) => postSave(r, pageContext), keepScreenLocked = true)
      }
      reactions += {case ButtonClicked(b) => saveEdits()}

      override protected def paintComponent(g:Graphics2D) {
        super.paintComponent(g)
        if (numEdits > 0) {
          g.setColor(Color.WHITE)
          g.setFont(GuiUtils.GuiFieldFilterNumberFont)

          val fm = g.getFontMetrics(GuiUtils.GuiFieldFilterNumberFont)
          val sWidth = fm.stringWidth(numEditsString)
          val startPosX = size.width - sWidth - 4
          val sHeight = fm.getHeight
          val startPosY = sHeight - 3

          g.fillRect(startPosX, 2, sWidth + 1, sHeight - 4)

          g.setColor(Color.RED)
          g.drawString(numEditsString, startPosX, startPosY)
        }
      }
    }
    val resetEditsButton = new ToolBarButton {
      icon = StarlingIcons.icon("/icons/16x16_undo.png")
      tooltip = "Reset all unsaved edits"
      enabled = edits.nonEmpty
      reactions += {case ButtonClicked(b) => {
        pageContext.goTo(selfPage(pivotPageState, PivotEdits.Null))
      }}
    }

    val clearPivotButton = new ToolBarButton {
      icon = StarlingIcons.icon("/icons/16x16_clear_pivot.png")
      tooltip = "Remove all fields from the pivot table"
      enabled = {
        currentFieldState.rowFields.nonEmpty || currentFieldState.columns.allFields.nonEmpty || currentFieldState.filters.nonEmpty
      }
      reactions += {
        case ButtonClicked(b) => {
          val pfs = PivotFieldsState.Blank.copy(reportSpecificChoices = currentFieldState.reportSpecificChoices)
          pageContext.goTo(selfPage(pivotPageState.copyPivotFieldsState(pfs), edits))
        }
      }
    }

    add(lockScreenButton)
    add(expandColumnsToFitButton)
    add(removeZerosButton)
    addSeparator()
    add(clearPivotButton)
    add(rotateButton)
    addSeparator()
    add(bottomTotalsButton)
    add(rowSubTotalsButton)
    add(rightTotalsButton)
    add(columnSubTotalsButton)
    addSeparator()
    add(chartButton)
    addSeparator()
    add(toggleCalculateButton)
    addSeparator()
    if (data.pivotTable.editableInfo == None) {
      if (pageContext.localCache.version.production) {
        add(copyButton, "pushx")
      } else {
        add(copyButton)
        addSeparator()
        add(clearCacheButton, "pushx")
      }
    } else {
      add(copyButton)
      addSeparator()
      if (pageContext.localCache.version.production) {
        add(saveEditsButton)
        add(resetEditsButton, "pushx")
      } else {
        add(saveEditsButton)
        add(resetEditsButton)
        addSeparator()
        add(clearCacheButton, "pushx")
      }
    }

    for (button <- toolbarButtons) {
      add(button)
    }

    def addSeparator() {
      val separator = new Separator(Orientation.Vertical)
      add(separator, "pushy, growy, gapleft 2, gapright 1")
    }

    private def resetTotals() {
      bottomTotalsButton.selected = pivotPageState.otherLayoutInfo.totals.rowGrandTotal
      rightTotalsButton.selected = pivotPageState.otherLayoutInfo.totals.columnGrandTotal
      rowSubTotalsButton.selected = pivotPageState.otherLayoutInfo.totals.rowSubTotals
      columnSubTotalsButton.selected = pivotPageState.otherLayoutInfo.totals.columnSubTotals
    }

    def resetToolbarState() {
      lockScreenButton.selected = pivotPageState.otherLayoutInfo.frozen
      toggleCalculateButton.selected = !pivotPageState.pivotFieldParams.calculate
      resetTotals()
    }

    resetTotals()
  }
  pivotComp.setCustomToolBar(toolBar, () => toolBar.resetToolbarState())

  reactions += {
    case FieldsChangedEvent(pivotFieldState) => pageContext.goTo(selfPage(pivotPageState.copyPivotFieldsState(pivotFieldState), edits))
    case TableDoubleClickEvent(filterFields, drillDownFields, modifiers) => {

      def getAxis(fields:Seq[(Field,Selection)], drillDownInfo:DrillDownInfo) = {
        val axisToUse = drillDownInfo.fallBack
        drillDownInfo.filteredDrillDown match {
          case None => axisToUse
          case Some(ddInfo) => {
            val dDF = fields.filter(_._2 != AllSelection).map(_._1)
            val dDFMap = Map() ++ fields
            if (dDF.contains(ddInfo.filterField)) {
              // We are pivoting on something we know more information about. Look at the selection and get the fields for it.
              val selection = dDFMap(ddInfo.filterField)
              selection match {
                case AllSelection => {throw new IllegalStateException("This case hasn't been coded for yet: " + selection + " : filter field: " + ddInfo.filterField)}
                case SomeSelection(values) if values.isEmpty => {throw new IllegalStateException("This case hasn't been coded for yet: " + selection)}
                case SomeSelection(values) => {
                  val fieldsToUse = values.flatMap{
                    _ match {
                      case s:String => {
                        (Map() ++ ddInfo.fieldToFields)(s)
                      }
                      case _ => throw new IllegalStateException("This case hasn't been coded for yet (selection not a string): " + selection)
                    }}.toList
                  axisToUse.copy(dataFields = axisToUse.dataFields.intersect(fieldsToUse))
                }
                case _ => throw new IllegalStateException("This case hasn't been coded for yet: " + selection)
              }
            } else {
              axisToUse
            }
          }
        }
      }

      val allFieldsUsed = currentFieldState.allFieldsUsed
      val possibleGroups = drillDownGroups.map(group=>{
        // Generate the group to remove all the fields from.
        val axisToUse = getAxis(filterFields, group).removeAll(allFieldsUsed)
        getAxis(drillDownFields, DrillDownInfo(axisToUse, group.filteredDrillDown))
      }).filterNot(_.isEmpty)

      if (!possibleGroups.isEmpty && !currentFieldState.rowFields.contains(Field("Trade ID"))) {
        val newFieldState = currentFieldState.withFiltersAndRowFields(drillDownFields, possibleGroups.head)
        val newPPS = pivotPageState.copy(pivotFieldParams = pivotPageState.pivotFieldParams.copy(pivotFieldState = Some(newFieldState)))
        pageContext.goTo(selfPage(newPPS, edits), modifiers)
      } else {
        finalDrillDown(filterFields ++ drillDownFields, pageContext, modifiers)
      }
    }
    case FullScreenSelectedEvent(currentState, newState, currentFrozen) => {
      val newFrozen = if (newState == AllHidden) false else currentFrozen
      val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(frozen = newFrozen, hiddenType = newState, oldHiddenType = Some(currentState), oldFrozen = Some(currentFrozen))
      pageContext.goTo(selfPage(pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo), edits))
    }
    case ShowErrorsEvent(errors) => {pageContext.goTo(new ReportCellErrorsPage(errors.toList))}
    case PivotEditsUpdatedEvent(edits0, t) => pageContext.goTo(selfPage(pivotPageState, edits0), compToFocus = Some(t))
    case SavePivotEdits if toolBar.saveEditsButton.enabled => toolBar.saveEditsButton.saveEdits()
    case FieldPanelEvent(collapse) => {
      val hiddenType = if (collapse) FieldListHidden else NothingHidden
      val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(hiddenType = hiddenType)
      pageContext.goTo(selfPage(pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo), edits))
    }
    case CollapsedStateUpdated(rowOption, columnOption) => {
      val newOtherLayoutInfo = rowOption match {
        case Some(r) => pivotPageState.otherLayoutInfo.copy(rowCollapsedState = r)
        case None => pivotPageState.otherLayoutInfo.copy(columnCollapsedState = columnOption.get)
      }
      pageContext.goTo(selfPage(pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo), edits))
    }
    case SubTotalToggled(field, location) => {
      val subTotalsDisabled = pivotPageState.otherLayoutInfo.disabledSubTotals
      val newDisabledSubTotals = if (subTotalsDisabled.contains(field)) {
        subTotalsDisabled.filterNot(_ == field)
      } else {
        field :: subTotalsDisabled
      }
      val newOtherLayoutInfo = pivotPageState.otherLayoutInfo.copy(disabledSubTotals = newDisabledSubTotals)
      pageContext.goTo(selfPage(pivotPageState.copy(otherLayoutInfo = newOtherLayoutInfo), edits))
    }
    case UserSettingUpdated(StandardUserSettingKeys.ExtraFormattingInfo) => {
      val newExtraFormatInfo = pageContext.getSetting(StandardUserSettingKeys.ExtraFormattingInfo)
      pivotComp.extraFormatInfoUpdated(newExtraFormatInfo)
    }
  }
  listenTo(pivotComp, pageContext.remotePublisher)

  add(pivotTableComponent, "push, grow")

  override def restoreToCorrectViewForBack()  {pivotComp.reverse()}
  override def resetDynamicState()  {pivotComp.resetDynamicState()}
  def selection = {pivotComp.selection}
  override def getBorder = {
    if (pivotPageState.otherLayoutInfo.hiddenType == AllHidden) {
      Some(MatteBorder(0,1,0,0,GuiUtils.BorderColour))
    } else {
      super.getBorder
    }
  }

  override def setState(state:Option[ComponentState]) {
    state match {
      case Some(AbstractPivotComponentState(filterText, colScrollPos, mainScrollPos, rsScrollPos, selectedCells, configPanelState)) => {
        pivotComp.filterText = filterText
        pivotComp.setColScrollPos(colScrollPos)
        pivotComp.setMainScrollPos(mainScrollPos)
        pivotComp.setRSScrollPos(rsScrollPos)
        pivotComp.setSelectedCells(selectedCells)
        pivotComp.configPanelState = configPanelState
      }
      case _ =>
    }
  }

  override def getState = {
    Some(AbstractPivotComponentState(pivotComp.filterText, pivotComp.getColScrollPos,
      pivotComp.getMainScrollPos, pivotComp.getRSScrollPos, pivotComp.getSelectedCells, pivotComp.configPanelState))
  }

  override def getTypeState = {
    Some(AbstractPivotComponentTypeState(pivotComp.filterText, pivotComp.getRSScrollPos, pivotComp.getMainScrollPos,
      pivotComp.configPanelState, pivotComp.getSelectedCells))
  }

  override def setTypeState(typeState:Option[ComponentTypeState]) {
    typeState match {
      case Some(AbstractPivotComponentTypeState(filterText, rsScrollPos, mainScrollPos, configPanelState, selectedCells)) => {
        pivotComp.filterText = filterText
        pivotComp.setRSScrollPos(rsScrollPos)
        pivotComp.configPanelState = configPanelState
        pivotComp.setSelectedCells(selectedCells)
        pivotComp.setMainScrollPos(mainScrollPos)
      }
      case _ =>
    }
  }

  override def getTypeFocusInfo = Some(AbstractPivotComponentTypeFocusInfo)
  override def setTypeFocusInfo(focusInfo:Option[TypeFocusInfo]) {
    focusInfo match {
      case Some(AbstractPivotComponentTypeFocusInfo) => pivotComp.updateFocusBasedOnCellSelection()
      case _ => 
    }
  }

  override def getRefreshInfo = Some(pivotComp.refreshInfo)

  override def defaultComponentForFocus = pivotComp.defaultComponentForFocus
  override def pageResized(newSize:Dimension) {pivotComp.pageResized(newSize)}
  def getSelection = selection
}

case object ClearServerSideCache extends SubmitRequest[Unit] {

  def baseSubmit(serverContext: ServerContext) { serverContext.lookup(classOf[ReportFacility]).clearCache}
}

case class AbstractPivotComponentState(filterText:String,
                                       colScrollPosition:Int,
                                       mainScrollPosition:Point,
                                       reportSpecificScrollPosition:Int,
                                       selectedCells:Either[List[(Int,Int)], (List[(Int,Int)],List[(Int,Int)],List[(Int,Int)])],
                                       configPanelState:Option[NTabbedPaneState]) extends ComponentState
case class AbstractPivotComponentTypeState(filterText:String, reportSpecificScrollPosition:Int, mainScrollPosition:Point,
                                           configPanelState:Option[NTabbedPaneState],
                                           selectedCells:Either[List[(Int,Int)],
                                           (List[(Int,Int)],List[(Int,Int)],List[(Int,Int)])]) extends ComponentTypeState

case object AbstractPivotComponentTypeFocusInfo extends TypeFocusInfo