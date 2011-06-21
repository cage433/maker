package starling.gui.pages

import starling.gui._
import starling.gui.GuiUtils._
import starling.pivot.view.swing.{NListView, MigPanel}
import java.awt.{Color, Dimension}
import starling.gui.api.UserLoggedIn
import swing.{TextField, Label, Button, ScrollPane}
import swing.event.{ListSelectionChanged, ButtonClicked}

case class RunAsUserPage() extends Page {
  def text = "Run as another user"
  def icon = StarlingIcons.im("/icons/16x16_user_dark.png")
  def build(reader:PageBuildingContext) = null
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension) = new RunAsUserPageComponent(context)
}

class RunAsUserPageComponent(context:PageContext) extends MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p][p]unrel[grow]") with PageComponent {
  private val userField = new TextField(20)

  private val currentUserNames = context.localCache.allUserNames.toList
  private val userListView = new NListView(currentUserNames) {
    if (currentUserNames.nonEmpty) {
      val currentUser = currentUserNames(0)
      selected = currentUser
      userField.text = currentUser
    }
    preferredSize = new Dimension(preferredSize.width + 30, preferredSize.height)
  }

  reactions += {
    case UserLoggedIn(user) => {
      val uName = user.username
      val currentData = userListView.listData.toList
      if (!currentData.contains(uName)) {
        // Update the list view.
        val currentSelected = userListView.selected
        userListView.listData = (uName :: currentData).sorted
        userListView.selected = currentSelected
      }
    }
    case ListSelectionChanged(`userListView`,_,false) => {
      val selectedUser = userListView.selection.items.headOption match {
        case None => ""
        case Some(u) => u
      }
      userField.text = selectedUser
    }
  }
  listenTo(context.remotePublisher, userListView.selection)

  private val usersScroll = new ScrollPane(userListView)
  private val runButton = new Button {
    text = "Run As User"
    enabled = currentUserNames.nonEmpty
    reactions += {
      case ButtonClicked(b) => {
        if (context.localCache.isStarlingDeveloper) {
          errorLabel.text = ""
          val userName = userField.text.trim
          new Thread {
            override def run = {
              Thread.currentThread().setContextClassLoader(RunAsUserPage.getClass.getClassLoader)
              Launcher.start(Launcher.rmiHost,Launcher.rmiPort,Launcher.servicePrincipalName,Some(userName))
            }
          }.start
        } else {
          errorLabel.text = "<html><b>You do not have permission to change to a different user</b></html>"
        }
      }
    }
  }
  private val errorLabel = new Label("") {
    foreground = Color.RED
  }

  add(LabelWithSeparator("Select User to Run As"), "spanx, growx, wrap")
  add(userField, "skip 1, sgx")
  add(runButton, "ay top")
  add(errorLabel, "ay top, wrap")
  add(usersScroll, "skip 1, pushy, growy, sgx")

  override def pageShown = runButton.requestFocusInWindow
}