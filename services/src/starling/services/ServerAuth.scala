package starling.services

import starling.utils.Log
import starling.auth.{User, LdapUserLookup, ServerLogin}
import starling.bouncyrmi.{BouncyLdapUserLookup, AuthHandler, ServerAuthHandler}
import java.util.UUID

class ServerAuth(login: ServerLogin, ldap: LdapUserLookup with BouncyLdapUserLookup[User], users:java.util.Map[UUID,User],
                 userConnected:(User) => Unit) {

  // The ticket username comes in the form of an email address
  val UserName = """([\w._-]+)@[\w.-]+\.[\w]{2,4}""".r

  def handler: ServerAuthHandler[User] = {
    val subject = login.login

    new ServerAuthHandler(new AuthHandler[User] {
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
    }, users, ldap, userConnected, ChannelLoggedIn)
  }
}