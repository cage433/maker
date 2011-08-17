package starling.gui

import api.{UserReportData}
import javax.swing.event.{DocumentEvent, DocumentListener}
import swing._
import event.{MousePressed, ButtonClicked}
import java.awt.{Dimension, Color, Polygon, RenderingHints, KeyboardFocusManager}
import xstream.GuiStarlingXStream
import starling.rmi.StarlingServer
import starling.pivot.PivotLayout
import javax.swing.{JPopupMenu, BorderFactory}
import swing.Swing._
import starling.gui.StarlingLocalCache._
import starling.browser.{PageContext, Bookmark}
import starling.browser.common.{GuiUtils, MigPanel}



case class DeleteReportRequest(reportName:String) extends StarlingSubmitRequest[Unit] {
  def submit(serverContext:StarlingServerContext) {
    serverContext.server.deleteLayout(reportName)
    serverContext.server.deleteUserReport(reportName)
  }
}