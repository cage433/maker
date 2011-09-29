package starling.gui.pages

import starling.auth.User
import starling.gui._
import swing.Label
import java.awt.{Color, Font, Dimension}
import starling.browser.common._
import starling.browser._

case class UserDetailsPage(user:User) extends StarlingServerPage {
  def text = "User Details"
  def icon = StarlingIcons.im("/icons/16x16_user_dark.png")

  def build(reader:StarlingServerContext) = null
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new UserDetailsPageComponent(context:PageContext, user:User)
}

class UserDetailsPageComponent(context:PageContext, user:User) extends MigPanel("insets " + GuiUtils.StartPageInsets) with PageComponent {
  background = Color.WHITE
  val c = new StripedPanel("insets 0", "push[" + GuiUtils.StandardLeftIndent + "][p]unrel[p]push", "push[p]unrel[p][p][p][p]push[p]") {
    def titleLabel(text0:String) = new Label {
      text = text0
      font = new Font("Serif", Font.PLAIN, 20)
    }

    def sL(text0:String) = new Label {
      text = text0
      foreground = GuiUtils.BlueTextColour
      horizontalAlignment = swing.Alignment.Right
    }

    def vL(text0:String) = new Label(text0)

    val namePanel = new MigPanel("insets n 0 n n") {
      background = new Color(0,0,0,0)
      opaque = false

      val userImage = StarlingIcons.im("/icons/32x32_user_dark.png")
      val userImagePanel = new FixedImagePanel(userImage)

      add(userImagePanel)
      add(titleLabel(user.name), "gapleft unrel")
    }

    val orgImage = StarlingIcons.im("/icons/32x32_organisation.png")
    val orgButton = new NumberedButton("Organisation", orgImage, modifiers => {
      context.goTo(OrgPage(PivotPageState()), modifiers)
    })

    add(namePanel, "spanx 3, wrap")

    add(sL("phone"), "growx, skip 1")
    add(vL(user.phoneNumber), "wrap")

    add(sL("email"), "growx, skip 1")
    add(vL(user.email), "wrap")

    add(sL("manager"), "growx, skip 1")
    add(vL(user.manager.getOrElse("Unknown")), "wrap")

    add(sL("department"), "growx, skip 1")
    add(vL(user.department), "wrap")

    add(orgButton, "split, spanx, ax center, gapbottom 5lp")
  }
  add(c, "push,grow")
}