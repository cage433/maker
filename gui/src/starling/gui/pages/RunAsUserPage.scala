package starling.gui.pages

import java.awt.{Color, Dimension}
import swing.{TextField, Label, Button, ScrollPane}
import swing.event.{MouseClicked, KeyPressed, ListSelectionChanged, ButtonClicked}
import starling.browser.common.{NListView, MigPanel}
import starling.browser.common.GuiUtils._
import starling.browser._
import service.UserLoggedIn
import starling.gui.StarlingLocalCache._
import starling.gui.{StarlingIcons}
import starling.gui.osgi.StarlingBrowserBundle

case class RunAsUserPage() extends Page {
  def text = "Run as another user"
  def icon = StarlingIcons.im("/icons/16x16_user_dark.png")
  def bundle = StarlingBrowserBundle.BundleName
  def build(serverContext: String) = null
  type SC = String
  def createServerContext(sc: ServerContext) = ""

  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new RunAsUserPageComponent(context)
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

  private def runAsUser() {
    if (context.localCache.isStarlingDeveloper) {
      errorLabel.text = ""
      val userName = userField.text.trim
      new Thread {
        override def run() {
          Thread.currentThread().setContextClassLoader(RunAsUserPage.getClass.getClassLoader)
          val launcherKlass = getClass.getClassLoader.loadClass("starling.launcher.Launcher")
          val startWithUserMethod = launcherKlass.getMethod("startWithUser", classOf[String])
          startWithUserMethod.invoke(null, userName)
        }
      }.start()
    } else {
      errorLabel.text = "<html><b>You do not have permission to change to a different user</b></html>"
    }
  }

  reactions += {
    case UserLoggedIn(username) => {
      val uName = username
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
    case KeyPressed(`userListView`, scala.swing.event.Key.Enter, _,_) => runAsUser()
    case MouseClicked(`userListView`, _,_,2,_) => runAsUser()
  }
  listenTo(context.remotePublisher, userListView.selection, userListView.keys, userListView.mouse.clicks)

  private val usersScroll = new ScrollPane(userListView)
  private val runButton = new Button {
    text = "Run As User"
    enabled = currentUserNames.nonEmpty
    mnemonic = swing.event.Key.R
    reactions += {
      case ButtonClicked(b) => runAsUser()
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

  override def defaultComponentForFocus = Some(userListView.peer)
}