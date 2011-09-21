package starling.auth

import starling.manager.DoNotCache

/**
 * AuthHandler provides a basis for Starling server authentication handlers using an optional ticket and optional "sudo"
 * overrides.
 *
 * @documented
 */
trait AuthHandler {
  /**
   * If authorized a User should be returned. Otherwise
   * None.
   */
  @DoNotCache def authorized(ticket: Option[Array[Byte]], sudo:Option[String]): Option[User] = ticket.flatMap(authorized(_, sudo))

  /**
   * Authorizes the given ticket by resolving it to a User.  If the sudo flag is set, then the usual authorization
   * method may be overridden in favour of another specified by the implementation.
   * @param The ticket, may not be null.
   * @param The optional "sudo" override.
   * @return The authorized user, otherwise None.
   */
  @DoNotCache def authorized(ticket: Array[Byte], sudo:Option[String]): Option[User]

  /**
   * Creates an authorizer that provides a callback with the user prior to returning it.
   *
   * @return The callback authorizer.
   */
  @DoNotCache def withCallback(callback : Option[User] => Unit) = new CallbackAuthHandler(this, callback)
}

/**
 * A singleton implementation providing two special AuthHandler instances.
 *
 * @documented
 */
object AuthHandler {
  /**
   * An AuthHandler that never authorizes.
   */
  val Never= new AuthHandler {
    /** @return None. */
    def authorized(ticket: Array[Byte], sudo:Option[String]) = None
  }
  /**
   * An AuthHandler that only authorizes if the optional sudo is defined.
   */
  val Dev = new AuthHandler {
    /** @return User.Dev if sudo is defined, None otherwise. */
    def authorized(ticket: Array[Byte], sudo:Option[String]) = {
      if (sudo.isDefined) throw new Exception("Dev auth handler does not support sudo")
      Some(User.Dev)
    }
  }
}

/**
 * 
 */
class CallbackAuthHandler(delegate: AuthHandler, callback: Option[User] => Unit) extends AuthHandler {
  def authorized(ticket: Array[Byte], sudo:Option[String]) = {
    val user = delegate.authorized(ticket, sudo)
    callback(user)
    user
  }
}
