package starling.gui.pages

import starling.gui._
import api.ReferenceDataLabel
import starling.pivot.PivotFieldParams
import swing.Swing._
import java.awt.image.BufferedImage
import java.awt.{Cursor, Color, Dimension}
import swing.event.{MouseExited, MouseEntered, MouseClicked}
import starling.pivot.view.swing.{StripedPanel, FixedImagePanel, MigPanel}
import javax.swing.{KeyStroke, JComponent}
import java.awt.event.KeyEvent
import swing.{Action, Label}

/**
 * The reference data pages for viewing markets, calendars etc.
 */

case object ReferenceDataIndexPage extends Page {
  def text = "Reference Data"
  val icon = StarlingIcons.im("/icons/16x16_ref_data.png")
  def build(reader: PageBuildingContext) = ReferenceDataIndexPageData(reader.starlingServer.referenceDataTables(), reader.starlingServer.permissionToDoAdminLikeThings)
  def createComponent(context: PageContext, data: PageData, browserSize: Dimension) = {ReferenceDataIndexPageComponent(context, data)}
}

case class ReferenceDataIndexPageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets dialog") with PageComponent {
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
      val tableButton = new ReferenceDataButton(table.name, imageToUse,
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
  override def layoutType = Some("ReferenceData")
  override val icon = if (table.name.toLowerCase.trim != "calendars") {
    StarlingIcons.im("/icons/16x16_chart_line.png")
  } else {
    StarlingIcons.im("/icons/16x16_calendar_day.png")
  }
  def selfPage(pivotPageStateX: PivotPageState) = copy(pivotPageState=pivotPageStateX)
  def dataRequest(pageBuildingContext: PageBuildingContext) = {
    pageBuildingContext.starlingServer.referencePivot(table, pivotPageState.pivotFieldParams)
  }
}

class ReferenceDataButton(text:String, image:BufferedImage, buttonClicked:(Boolean) => Unit, useBlueText:Boolean=true,
                          number:Option[String]=None) extends MigPanel {
  background = GuiUtils.TaskPageButtonBackgroundColour
  border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val imagePanel = new FixedImagePanel(image)
  val label = if (useBlueText) {
    new Label("<html><u>" + text + "</u></html>") {
      foreground = Color.BLUE
      name = text
    }
  } else {
    new Label(text)
  }

  add(imagePanel)
  number match {
    case None =>
    case Some(num) => if (useBlueText) {
      add(new Label("<html><u>" + num + "</u></html>") {
        foreground = Color.BLUE
      }, "split, gapright 2lp")
    } else {
      add(new Label(num), "split, gapright 2lp")
    }
  }
  add(label)

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    imagePanel.enabled = b
    label.enabled = b
  }

  reactions += {
    case MouseClicked(_,_,k,_,_) if enabled => {
      buttonClicked(k == scala.swing.event.Key.Modifier.Control)
      background = GuiUtils.TaskPageButtonBackgroundColour
    }
    case MouseEntered(_,_,_) if enabled => {background = GuiUtils.TaskPageButtonOverBackgroundColour}
    case MouseExited(_,_,_) if enabled => {background = GuiUtils.TaskPageButtonBackgroundColour}
  }
  listenTo(mouse.clicks, mouse.moves)
}
