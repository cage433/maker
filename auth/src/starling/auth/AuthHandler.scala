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
      if (sudo.isDefined) throw new Exception("Dev auth handler does not support sudo")
      Some(User.Dev)
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
