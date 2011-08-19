package starling.gui.pages

import starling.gui._
import api.ReferenceDataLabel
import swing.Swing._
import java.awt.image.BufferedImage
import java.awt.{Cursor, Color, Dimension}
import swing.event.{MouseExited, MouseEntered, MouseClicked}
import javax.swing.{KeyStroke, JComponent}
import java.awt.event.KeyEvent
import swing.{Action, Label}
import starling.pivot.{PivotEdits, PivotFieldParams}
import starling.browser.{PageComponent, Bookmark, PageData, PageContext}
import starling.browser.common._

/**
 * The reference data pages for viewing markets, calendars etc.
 */

case object ReferenceDataIndexPage extends StarlingServerPage {
  def text = "Reference Data"
  def icon = StarlingIcons.im("/icons/16x16_ref_data.png")
  def build(starlingServerContext: StarlingServerContext) = ReferenceDataIndexPageData(starlingServerContext.server.referenceDataTables(), starlingServerContext.server.permissionToDoAdminLikeThings)
  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = {new ReferenceDataIndexPageComponent(context, data)}
}

class ReferenceDataIndexPageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets dialog") with PageComponent {
  val data = pageData match {case d:ReferenceDataIndexPageData => d}

  val c = new StripedPanel("insets 0", "[grow][p][p][p][grow]", "[grow][p][p][p][p][grow 150]") {
    val portNumberString = (data.referenceTables.size + 1).toString + "."
    ReferenceDataIndexPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
            put(KeyStroke.getKeyStroke(KeyEvent.VK_1 + data.referenceTables.size, 0), portNumberString)

    val calendarImage = StarlingIcons.im("/icons/32x32_calendar.png")
    val otherRefDataImage = StarlingIcons.im("/icons/32x32_chart_line.png")
    for ((table,index) <- data.referenceTables.zipWithIndex) {
      val imageToUse = if (table.name.toLowerCase.trim != "calendars") {
        otherRefDataImage
      } else {
        calendarImage
      }
      val shouldSkip2 = (table.name.toLowerCase.trim == "calendars")
      val numberString = (index + 1).toString + "."
      def gotoPage(ctrlDown:Boolean) = context.goTo(ReferenceDataPage(table, PivotPageState(false, PivotFieldParams(true, None))), ctrlDown)
      val tableButton = new NumberedButton(table.name, imageToUse,
        gotoPage,
        number = Some(numberString))
      ReferenceDataIndexPageComponent.this.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).
              put(KeyStroke.getKeyStroke(KeyEvent.VK_1 + index, 0), numberString)
      ReferenceDataIndexPageComponent.this.peer.getActionMap.put(numberString, Action(numberString){gotoPage(false)}.peer)
      val constraints = if (index % 3 == 0) {
        if (shouldSkip2) "newline, skip 2" else "newline, skip 1"
      } else {
        ""
      }
      add(tableButton, constraints + " ,sg")
    }

  }

  add(c, "push, grow")
}

case class ReferenceDataIndexPageData(referenceTables:List[ReferenceDataLabel], admin: Boolean) extends PageData

case class ReferenceDataPage(table:ReferenceDataLabel, pivotPageState : PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def text = table.name
  override def icon = if (table.name.toLowerCase.trim != "calendars") {
    StarlingIcons.im("/icons/16x16_chart_line.png")
  } else {
    StarlingIcons.im("/icons/16x16_calendar_day.png")
  }
  def selfPage(pivotPageStateX: PivotPageState, edits:PivotEdits) = copy(pivotPageState=pivotPageStateX)
  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.server.referencePivot(table, pivotPageState.pivotFieldParams)
  }
}