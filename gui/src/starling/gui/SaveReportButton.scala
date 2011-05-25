package starling.gui

import api.{PivotLayoutUpdate, UserReportData, UserReportUpdate}
import pages.PivotPageState
import swing.event.ButtonClicked
import starling.pivot.view.swing.MigPanel
import starling.pivot.PivotLayout
import javax.swing.BorderFactory
import java.awt.{Color, Dimension}
import swing._
import starling.rmi.PivotData
import starling.auth.User
import javax.swing.event.{DocumentEvent, DocumentListener}

class SaveReportButton(pageContext:PageContext, userReportData:Option[UserReportData], pivotData:PivotData,
                       pivotPageState:PivotPageState, showParameters:Boolean, user0:User, layoutType:String) extends ToolBarButton {
  def checkIsReportKnown() = {
    userReportData match { case Some(rd) => pageContext.localCache.userReports.map(_.data).contains(rd); case None => false }
  }

  var knownReport = checkIsReportKnown()
  def refresh {
    if (knownReport) {
      tooltip = "Delete the current report configuration"
      icon = StarlingIcons.icon("/icons/16x16_star_remove.png")
    } else {
      tooltip = "Save the current report configuration"
      icon = StarlingIcons.icon(StarlingIcons.SaveReportConfiguration)
    }
  }
  refresh

  def setSavePanel(setup:Boolean) {
    holderPanel.update(savePanel, setup)
    if (setup) {
      savePanel.nameField.requestFocusInWindow
      savePanel.nameField.selectAll
      pageContext.setDefaultButton(Some(savePanel.okButton))
    }
  }
  def setReplacePanel {
    holderPanel.update(replacePanel, true)
  }
  def getText:String = savePanel.nameField.text
  def clearUp:Unit = savePanel.clearUp

  def customLayout = {
    val fieldsState = pivotData.pivotFieldsState
    val otherLayoutInfo = pivotPageState.otherLayoutInfo
    val layouts = pageContext.localCache.userPivotLayouts
    !layouts.map(l => {(l.pivotFieldState,l.otherLayoutInfo)}).contains((fieldsState, otherLayoutInfo))
  }

  // This is only called when it is not a custom layout.
  def currentLayout = {
    val fieldsState = pivotData.pivotFieldsState
    val otherLayoutInfo = pivotPageState.otherLayoutInfo
    val layouts = pageContext.localCache.userPivotLayouts
    layouts.find(l => {(l.pivotFieldState == fieldsState) && (l.otherLayoutInfo == otherLayoutInfo)}).get
  }

  def getLayoutInfo:(PivotLayout, Boolean, Boolean) = {
    if (customLayout) {
      val layouts = pageContext.localCache.userPivotLayouts
      def validName(s:String) = {
        s.nonEmpty && !layouts.map(_.layoutName.trim).contains(s)
      }
      val startName = savePanel.layoutTextField.text
      var name = startName
      if (!validName(name)) {
        var badName = true
        var count = 1
        while (badName) {
          name = startName + "(" + count + ")"
          if (validName(name)) {
            badName = false
          }
          count += 1
        }
      }
      val saveAndAssociate = savePanel.layoutCheckBox.selected
      (PivotLayout(name, pivotData.pivotFieldsState, true, pivotPageState.otherLayoutInfo, layoutType, List()), saveAndAssociate, saveAndAssociate)
    } else {
      (currentLayout, false, savePanel.layoutCheckBox.selected)
    }
  }
  def getReport = pageContext.localCache.userReports.find(_.data == userReportData.get).get

  val replacePanel = new MigPanel {
    border = BorderFactory.createLineBorder(new Color(158,16,40), 2)
    private val questionIcon = new Label {
      icon = StarlingIcons.icon("/icons/128x128_question.png")
    }
    val label = new Label("A report already exists with that name") {
      horizontalAlignment = Alignment.Left
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val textArea = starling.gui.GuiUtils.LabelTextArea("Would you like to replace it?")
    val yesButton = new Button {
      text = "Yes"
      reactions += {
        case ButtonClicked(e) => {
          def saveReport(a:Unit) {
            val (layoutName,shouldSaveLayout, shouldAssociate) = getLayoutInfo
            pageContext.submit(SaveReportRequest(getText.trim, userReportData.get, layoutName, shouldSaveLayout, shouldAssociate, showParameters))
            clearUp
          }
          pageContext.submit(DeleteReportRequest(getText.trim), saveReport)
        }
      }
    }
    val noButton = new Button {
      text = "No"
      reactions += {
        case ButtonClicked(e) => {setSavePanel(true)}
      }
    }

    add(questionIcon, "spany")
    add(label, "pushx, growx, wrap unrel, w " + label.preferredSize.width)
    add(textArea, "push, grow, wrap unrel")
    add(yesButton, "skip 1, split, al right, sg button")
    add(noButton, "al right, sg button")
  }

  val savePanel = new MigPanel {
    var oldDefaultButton:Option[Button] = None

    val infoIcon = new Label {
      icon = StarlingIcons.icon("/icons/128x128_info.png")
    }
    border = BorderFactory.createLineBorder(new Color(158,16,40), 2)
    val label = new Label("Please Enter the Report Details") {
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val nameLabel = new Label("Report Name:") {
      peer.setDisplayedMnemonic(java.awt.event.KeyEvent.VK_R)
    }
    val nameField = new TextField(20) {
      peer.getDocument.addDocumentListener(new DocumentListener {
        def changedUpdate(e:DocumentEvent) = {}
        def removeUpdate(e:DocumentEvent) = {updateLayoutName}
        def insertUpdate(e:DocumentEvent) = {updateLayoutName}
        def updateLayoutName {
          if (customLayout) layoutTextField.text = text
        }
      })
    }
    nameLabel.peer.setLabelFor(nameField.peer)
    val okButton = new Button {
      text = "OK"
      reactions += {
        case ButtonClicked(e) => {
          val newReportName = nameField.text
          if (newReportName.nonEmpty) {
            val currentReportNames = pageContext.localCache.userReports.map(_.reportName.trim)
            if (!currentReportNames.contains(newReportName.trim)) {
              val (pivotLayout, shouldSaveLayout, shouldAssociate) = getLayoutInfo
              pageContext.submit(SaveReportRequest(newReportName.trim, userReportData.get, pivotLayout, shouldSaveLayout, shouldAssociate, showParameters))
              clearUp
            } else {
              // Show a replace dialog.
              holderPanel.update(replacePanel, true)
              pageContext.setDefaultButton(Some(replacePanel.yesButton))
              replacePanel.yesButton.requestFocusInWindow
            }
          }
        }
      }
    }
    val cancelButton = new Button {
      text = "Cancel"
      reactions += {
        case ButtonClicked(e) => {clearUp}
      }
    }

    val layoutCheckBox = new CheckBox("") {
      selected = true
      mnemonic = scala.swing.event.Key.S
      reactions += {case ButtonClicked(b) => {layoutTextField.enabled = (customLayout && selected)}}
    }

    val layoutLabel = new Label("Layout Name:") {
      peer.setDisplayedMnemonic(java.awt.event.KeyEvent.VK_L)
    }
    val layoutTextField = new TextField(20) {
      if (!customLayout) {
        text = currentLayout.layoutName
      }
    }
    layoutLabel.peer.setLabelFor(layoutTextField.peer)

    add(infoIcon, "spany")
    add(label, "spanx, wrap unrel")
    add(nameLabel, "gapleft unrel")
    add(nameField, "wrap unrel")
    add(layoutCheckBox, "split, spanx, wrap")
    add(layoutLabel, "gapleft unrel")
    add(layoutTextField, "wrap unrel")
    add(okButton, "split, spanx, al right bottom, sg button")
    add(cancelButton, "al right bottom, sg button")

    def clearUp() {
      pageContext.clearContent
      pageContext.setDefaultButton(oldDefaultButton)
      pageContext.requestFocusInCurrentPage
    }
  }

  val holderPanel = new MigPanel("insets 0") {
    def update(c:Component, setSize:Boolean) {
      removeAll
      if (setSize) {
        val widthToUse = math.max(c.preferredSize.width, size.width)
        val heightToUse = math.max(c.preferredSize.height, size.height)
        c.preferredSize = new Dimension(widthToUse, heightToUse)
      }
      refresh
      add(c, "push,grow")
      revalidate
      repaint
    }
  }

  def updateLayoutInfo {
    val cLayout = customLayout
    val layoutText = {
      if (cLayout) {
        "Save and associate layout with report"
      } else {
        "Associate layout with report"
      }
    }
    savePanel.layoutCheckBox.text = layoutText
    savePanel.layoutTextField.enabled = cLayout && savePanel.layoutCheckBox.selected
    if (!cLayout) {
      savePanel.layoutTextField.text = currentLayout.layoutName
    }
  }

  updateLayoutInfo

  reactions += {
    case ButtonClicked(b) => {
      if (knownReport) {
        pageContext.submitYesNo("Are you sure you want to delete the \"" + getReport.reportName + "\" report configuration?",
          "Layouts only associated with this report will be deleted as well",
          DeleteReportRequest(getReport.reportName), (u:Unit) => {false}, (u:Unit) => {})
      } else {
        val oldDefaultButton = pageContext.getDefaultButton
        savePanel.oldDefaultButton = oldDefaultButton
        setSavePanel(false)
        pageContext.setContent(holderPanel, Some(savePanel.clearUp))
        pageContext.setDefaultButton(Some(savePanel.okButton))
        savePanel.nameField.requestFocusInWindow
      }
    }
  }
  reactions += {
    case UserReportUpdate(user, userReports) if (user == user0.username) => {
      knownReport = checkIsReportKnown()
      refresh
      repaint
    }
    case PivotLayoutUpdate(username, layouts) if username == user0.username => updateLayoutInfo
  }
  listenTo(pageContext.remotePublisher)
}