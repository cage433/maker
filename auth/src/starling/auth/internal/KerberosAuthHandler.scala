package starling.auth.internal

import starling.utils.Log
import starling.auth.{User, LdapUserLookup, AuthHandler}
import java.util.logging.Logger

class KerberosAuthHandler(serverPassword:String, ldap:LdapUserLookup) extends AuthHandler {

  val login = new ServerLogin(serverPassword)

  // The ticket username comes in the form of an email address
  val UserName = """([\w._-]+)@[\w.-]+\.[\w]{2,4}""".r


  def authorized(ticket: Array[Byte], sudo:Option[String]) = {
    if (ticket.isEmpty) {
      None
    }
    val subject = login.login
    try {
      Server.verify(subject, ticket).flatMap { ticketUsername =>
        val UserName(username) = ticketUsername

        ldap.user(username) match {
          case None => None
          case Some(realUser) => {
            sudo match {
              case None => Some(realUser)
              case Some(sudoUsername) => {
                ldap.user(sudoUsername) match {
                  case None => throw new Exception("Server: Couldn't override user to " + sudoUsername + " as it was not found in ldap")
                  case Some(user) => Some(user.copy(realUsername=Some(realUser.username)))
                }
              }
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