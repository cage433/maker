package starling.auth.internal

import starling.utils.Log
import starling.auth.{User, LdapUserLookup, AuthHandler}
import java.util.logging.Logger

class KerberosAuthHandler(serverPassword:String, ldap:LdapUserLookup) extends AuthHandler {

  val login = new ServerLogin(serverPassword)

  // The ticket username comes in the form of an email address
  val UserName = """([\w._-]+)@[\w.-]+\.[\w]{2,4}""".r


  def authorized(ticket: Array[Byte], sudo:Option[String]) = {
    val subject = login.login
    try {
      Server.verify(subject, ticket).flatMap { ticketUsername =>
        val UserName(username) = ticketUsername
        val realUser = ldap.user(username)
        sudo match {
          case None => realUser
          case Some(oUser) => {
            //Logger.info("Server: Overriding User to " + oUser)
            ldap.user(oUser) match {
              case None => throw new Exception("Server: Couldn't override user to " + oUser + " as it was not found in ldap")
              case Some(u) => Some(u)
            }
          }
        }
      }
    }
    catch {
      case e => {
        Log.error("Failed to auth ticket", e)
        None
      }
    }

  }
}