package starling.bouncyrmi

import starling.utils.Log
import starling.utils.ImplicitConversions._
import org.jboss.netty.channel._
import starling.auth.{LdapUserLookup, User}

trait AuthHandler {
  /**
   * If authorized a User should be returned. Otherwise
   * None.
   */
  def authorized(ticket: Option[Array[Byte]]): Option[User]

  def withCallback(callback : Option[User] => Unit) = new CallbackAuthHandler(this, callback)
}

class NullAuthHandler(user: Option[User]) extends AuthHandler {
  def authorized(ticket: Option[Array[Byte]]) = user
}

class CallbackAuthHandler(delegate: AuthHandler, callback: Option[User] => Unit) extends AuthHandler {
  def authorized(ticket: Option[Array[Byte]]) = delegate.authorized(ticket).update(callback(_))
}

class ServerAuthHandler(auth: AuthHandler, users:java.util.Set[User], ldapUserLookup:LdapUserLookup, userConnected:(User) => Unit)
  extends SimpleChannelUpstreamHandler with AuthHandler {

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) = {
    val channel = ctx.getChannel
    BouncyRMI.loggedIn.get(channel) match {
      case null => {
        e.getMessage match {
          case AuthMessage(ticket, overriddenUser) => {
            Log.info("Server: User login message found")
            authorized(ticket) match {
              case Some(user) => {
                Log.info("Server: User auth successful for " + user)
                userConnected(user)
                val userToUse = overriddenUser match {
                  case None => user
                  case Some(oUser) => {
                    Log.info("Server: Overriding User to " + oUser)
                    ldapUserLookup.user(oUser) match {
                      case None => Log.warn("Server: Couldn't override user to " + oUser + ", reverting to " + user);user
                      case Some(u) => u
                    }
                  }
                }
                Log.info("Server: Actually using the user " + userToUse)
                BouncyRMI.loggedIn.set(channel, userToUse)
                users.add(userToUse)
                channel.write(AuthSuccessfulMessage)
              }
              case None => {
                if(ticket != null && ticket.isDefined) {
                  Log.info("Server: User auth failed, disconnecting client")
                } else {
                  Log.info("Server: User auth failed, no kerberos ticket sent, disconnecting client")
                }
                val future = channel.write(AuthFailedMessage)
                future.addListener(new ChannelFutureListener {
                  def operationComplete(future: ChannelFuture) = {
                    channel.close
                  }
                })
              }
            }
          }
          case m => {
            Log.error("Unrecognised message on unauthorized channel, closing: " + m)
            channel.close
          }
        }
      }
      case user => {
        User.setLoggedOn(Some(user))
        ctx.sendUpstream(e)
        User.setLoggedOn(None)
      }
    }
  }

  def authorized(ticket: Option[Array[Byte]]) = auth.authorized(ticket)
}

