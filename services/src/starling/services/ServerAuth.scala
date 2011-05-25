package starling.services

import starling.bouncyrmi.{AuthHandler, ServerAuthHandler}
import starling.utils.Log
import starling.auth.{User, LdapUserLookup, ServerLogin}

class ServerAuth(login: ServerLogin, ldap: LdapUserLookup, users:java.util.Set[User], userConnected:(User) => Unit) {

  // The ticket username comes in the form of an email address
  val UserName = """([\w._-]+)@[\w.-]+\.[\w]{2,4}""".r

  def handler: ServerAuthHandler = {
    val subject = login.login

    new ServerAuthHandler(new AuthHandler {
      def authorized(ticket: Option[Array[Byte]]) = {
        try {
          ticket match {
            case Some(t) => {
              new starling.auth.Server().verify(subject, t) match {
                case Some(user) => {
                  val UserName(username) = user
                  ldap.user(username)
                }
                case None => None
              }
            }
            case _ => None
          }
        }
        catch {
          case e => {
            Log.error("Failed to auth ticket", e)
            None
          }
        }

      }
    }, users, ldap, userConnected)
  }
}