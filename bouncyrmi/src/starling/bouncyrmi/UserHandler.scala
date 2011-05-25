package starling.bouncyrmi

object UserHandler {
  lazy private val user = new ThreadLocal[String]()

  def setCurrentUser(name: String) = user.set(name)

  def currentUser = {
    user.get
  }
}