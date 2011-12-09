package starling.auth

trait AuthHandler {
  /**
   * If authorized a User should be returned. Otherwise
   * None.
   */
  def authorized(ticket: Option[Array[Byte]], sudo:Option[String]): Option[User] = ticket.flatMap(authorized(_, sudo))
  def authorized(ticket: Array[Byte], sudo:Option[String]): Option[User]

  def withCallback(callback : Option[User] => Unit) = new CallbackAuthHandler(this, callback)
}

object AuthHandler {
  val Never= new AuthHandler {
    def authorized(ticket: Array[Byte], sudo:Option[String]) = None
  }
  val Dev = new AuthHandler {
    def authorized(ticket: Array[Byte], sudo:Option[String]) = {
      sudo match {
        case None => Some(User.Dev)
        case Some(username) => Some(User(username, username, Some(User.Dev.username)))
      }

    }
  }
}

class CallbackAuthHandler(delegate: AuthHandler, callback: Option[User] => Unit) extends AuthHandler {
  def authorized(ticket: Array[Byte], sudo:Option[String]) = {
    val user = delegate.authorized(ticket, sudo)
    callback(user)
    user
  }
}
