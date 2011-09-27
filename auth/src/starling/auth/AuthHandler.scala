package starling.auth

import starling.manager.DoNotCache

trait AuthHandler {
  /**
   * If authorized a User should be returned. Otherwise
   * None.
   */
  @DoNotCache def authorized(ticket: Option[Array[Byte]], sudo:Option[String]): Option[User] = ticket.flatMap(authorized(_, sudo))
  @DoNotCache def authorized(ticket: Array[Byte], sudo:Option[String]): Option[User]

  @DoNotCache def withCallback(callback : Option[User] => Unit) = new CallbackAuthHandler(this, callback)
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
