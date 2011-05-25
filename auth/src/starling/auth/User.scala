package starling.auth

import org.springframework.ldap.core.ContextSource
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
  val Dev = User("Dev", "Dev", None, List("Starling Developers"), "+44 888 8888", "dev@trafigura.com", "IT Risk Systems")
  // Test user for automated tests
  val Test = User("Nobody", "Nobody", None, List(), "None", "None@trafigura.com", "None")

  /**
   * The user who is currently logged on.
   *
   * This uses threadlocal to store this but since every request runs in it's own thread this works fine.
   */
  def currentlyLoggedOn = loggedOn.get.assert(user => user != null, "No user logged on")

  private val loggedOn = new ThreadLocal[User]()

  def setLoggedOn(user: Option[User]) {
    if (user.isDefined) {
      assert(loggedOn.get == null, "User already set: " + User.loggedOn.get)
    }
    loggedOn.set(user)
  }

  def pushUser(user: Option[User]) = userRestorer(loggedOn.pop).update(restorer => setLoggedOn(user))
  private def userRestorer(previousUser: User) = ClosureUtil.closable(loggedOn.set(previousUser))
}



class LdapUserLookup extends Ldap {
  val cache = CacheFactory.getCache("LdapUser")

  def user(username: String): Option[User] = {
    cache.memoize(username.toLowerCase, {
      var user: Option[User] = None
      query("(&(objectCategory=person)(sAMAccountName=" + username + "))", m => {
        try {
          user = Some(parseUser(m))
          Log.info("LDAP username lookup of '" + username + "' produced: " + user.get.name)
        } catch {
          case e => {
            Log.warn("Failed to get user for username: " + username, e)
          }
        }
      })
      user
    })
  }

  def objectSID(username: String): Option[String] = {
    var sid:Option[String] = None

    query("(&(objectCategory=person)(sAMAccountName=" + username + "))", m => {
      val str = m("objectSid:").head
      val decoder = new BASE64Decoder
      val SID = decoder.decodeBuffer(str)
      val strSID: StringBuilder = new StringBuilder("S-")
      strSID.append(SID(0)).append('-')
      val tmpBuff: StringBuilder = new StringBuilder
      for (t <- 2 to 7) {
        tmpBuff.append(Integer.toHexString(SID(t) & 0xFF))
      }
      strSID.append(java.lang.Long.parseLong(tmpBuff.toString, 16))

      for (i <- 0 until SID(1).toInt) {
        val currSubAuthOffset: Int = i * 4
        tmpBuff.setLength(0)
        val str = String.format("%02X%02X%02X%02X", (SID(11 + currSubAuthOffset) & 0xFF).asInstanceOf[Object],
          (SID(10 + currSubAuthOffset) & 0xFF).asInstanceOf[Object], (SID(9 + currSubAuthOffset) & 0xFF).asInstanceOf[Object],
          (SID(8 + currSubAuthOffset) & 0xFF).asInstanceOf[Object])
        tmpBuff.append(str)
        strSID.append('-').append(java.lang.Long.parseLong(tmpBuff.toString, 16))
      }
      sid = Some(strSID.toString)
    })
    sid
  }

  def users = {
    var users: List[User] = List()
    query("(&(objectCategory=person)(showinaddressbook=*)(sAMAccountName=*)(givenname=David))", m => {
      try {
        val user = parseUser(m)
        println(user)
        users ::= user
      } catch {
        case e => {
          Log.warn("Failed to get user", e)
        }
      }
    })
    users
  }

  def usersStartingWith(letter:Char) = {
    val users = new ListBuffer[User]
    query("(&(objectCategory=person)(showinaddressbook=*)(sAMAccountName=*)(givenname=" + letter + "*))", m => {
      try {
        val user = parseUser(m)
        users += user
      } catch {
        case e => {
          Log.warn("Failed to get user", e)
        }
      }
    })
    users.toList
  }

  private val CN = """.*CN=([ '\w-]+).*""".r

  private def parseUser(m: Map[String, Set[String]]) = {
    val username = m("sAMAccountName").head
    val name = m("cn").head
    val manager = m.get("manager") match {
      case Some(s: Set[_]) => {
        val CN(manager) = s.head
        Some(manager)
      }
      case None => None
    }
    val groups = m.get("memberOf") match {
      case Some(s: Set[_]) => s map {
        a: String => {
          val CN(group) = a
          group.i
        }
      }
      case _ => Set()
    }
    val phoneNumber = m.getOrElse("telephoneNumber", Set("")).headOption.getOrElse("")
    val email = m.getOrElse("mail", Set("")).headOption.getOrElse("")
    val department = m.getOrElse("department", Set("")).headOption.getOrElse("")

    User(username, name, manager, groups.toList, phoneNumber, email, department)
  }

}