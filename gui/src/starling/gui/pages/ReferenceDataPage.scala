package starling.gui.pages

import starling.gui._
import api.ReferenceDataLabel
import java.awt.{Color, Dimension}
import javax.swing.{KeyStroke, JComponent}
import java.awt.event.KeyEvent
import swing.Action
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.browser.common._
import starling.browser._

/**
 * The reference data pages for viewing markets, calendars etc.
 */

case object ReferenceDataIndexPage extends StarlingServerPage {
  def text = "Reference Data"
  def icon = StarlingIcons.im("/icons/16x16_ref_data.png")
  def build(starlingServerContext: StarlingServerContext) = ReferenceDataIndexPageData(starlingServerContext.server.referenceDataTables())
  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = {new ReferenceDataIndexPageComponent(context, data)}
}

class ReferenceDataIndexPageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets " + GuiUtils.StartPageInsets) with PageComponent {
  val data = pageData match {case d:ReferenceDataIndexPageData => d}
  background = Color.WHITE

  val buttonPanel = new MigPanel("insets 0") {
    opaque = false

    val calendarImage = StarlingIcons.im("/icons/32x32_calendar.png")
    val otherRefDataImage = StarlingIcons.im("/icons/32x32_chart_line.png")
    for ((table,i) <- data.referenceTables.zipWithIndex) {
      val imageToUse = if (table.name.toLowerCase.trim != "calendars") {
        otherRefDataImage
      } else {
        calendarImage
      }
      val numberString = (i + 1).toString + "."
      def gotoPage(modifiers:Modifiers) {context.goTo(ReferenceDataPage(table, PivotPageState(false, PivotFieldParams(true, None))), modifiers)}
      val tableButton = new NumberedButton(table.name, imageToUse,
        gotoPage,
        number = Some(numberString))
      ReferenceDataIndexPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
              put(KeyStroke.getKeyStroke(KeyEvent.VK_1 + i, 0), numberString)
      ReferenceDataIndexPageComponent.this.peer.getActionMap.put(numberString, Action(numberString){gotoPage(Modifiers.None)}.peer)
      val extraConstraints = if (((i % 3) == 0) && (i != 0)) {
        ",newline, split 3, spanx"
      } else {
        ""
      }
      add(tableButton, "ax center, sg" + extraConstraints)
    }
  }

  val c = new StripedPanel("insets 0", "[grow][p][grow]", "[grow][p][grow 150]") {
    add(buttonPanel, "newline, skip 1")
  }
  add(c, "push, grow")
}

case class ReferenceDataIndexPageData(referenceTables:List[ReferenceDataLabel]) extends PageData

case class ReferenceDataPage(table:ReferenceDataLabel, pivotPageState : PivotPageState) extends AbstractStarlingPivotPage(pivotPageState) {
  def text = table.name
  override def icon = if (table.name.toLowerCase.trim != "calendars") {
    StarlingIcons.im("/icons/16x16_vols.png")
  } else {
    StarlingIcons.im("/icons/16x16_calendar_day.png")
  }
  def selfPage(pivotPageStateX: PivotPageState, edits:PivotEdits) = copy(pivotPageState=pivotPageStateX)
  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.server.referencePivot(table, pivotPageState.pivotFieldParams)
  }
}