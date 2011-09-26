package starling.auth

import javax.naming.directory.{BasicAttribute, Attribute}
import scala.collection.JavaConversions._
import collection.mutable.ListBuffer
import starling.utils.cache.CacheFactory
import sun.misc.BASE64Decoder
import starling.utils.CaseInsensitive._
import starling.utils.ImplicitConversions._
import starling.utils.{ClosureUtil, CaseInsensitive, Log}

case class User(username: String, name: String="", manager: Option[String]=None, groups: List[CaseInsensitive]=List(), phoneNumber:String="", email:String="", department:String="") {
  override def toString = username

  override def equals(obj: Any) = obj match {
    case other: User => username.toLowerCase == other.username.toLowerCase
    case s: String => username.toLowerCase == s
    case _ => false
  }

  override def hashCode = username.toLowerCase.hashCode

  def equalsIgnoreCase(name : String) = {
    name.equalsIgnoreCase(username)
  }
}

object User {
  // Dev user for use during development
  val Dev = User("Dev", "Dev name", None, List("Starling Developers"), "+44 888 8888", "dev@trafigura.com", "IT Risk Systems")
  // Test user for automated tests
  val Test = User("Nobody", "Nobody", None, List(), "None", "None@trafigura.com", "None")

  /**
   * The user who is currently logged on.
   *
   * This uses threadlocal to store this but since every request runs in it's own thread this works fine.
   */
  def currentlyLoggedOn = optLoggedOn.getOrElse(throw new java.lang.AssertionError("No user logged on"))
  def optLoggedOn: Option[User] = Option(loggedOn.get)

  private val loggedOn = new ThreadLocal[User]()

  def setLoggedOn(user: Option[User]) {
    if (user.isDefined) {
      assert(loggedOn.get == null, "User already set: " + loggedOn.get)
    }
    loggedOn.set(user)
  }

  def pushUser(user: Option[User]) = userRestorer(loggedOn.pop).update(restorer => setLoggedOn(user))
  private def userRestorer(previousUser: User) = ClosureUtil.closable(loggedOn.set(previousUser))
}



trait LdapUserLookup {
  def user(username: String): Option[User]
}
