package starling.bouncyrmi

import org.jboss.netty.channel._
import java.util.UUID

object Logger {
  def info(s: String, cause: Throwable = null) {
    println(s)
    if (cause != null) cause.printStackTrace()
  }
  def warn(s: String, cause: Throwable = null) {
    println(s)
    if (cause != null) cause.printStackTrace()
  }
  def error(s: String, cause: Throwable = null) {
    println(s)
    if (cause != null) cause.printStackTrace()
  }
}

trait BouncyLdapUserLookup[User] {
  def user(userName: String): Option[User]
}

trait AuthHandler[User] {
  /**
   * If authorized a User should be returned. Otherwise
   * None.
   */
  def authorized(ticket: Option[Array[Byte]]): Option[User]

  def withCallback(callback : Option[User] => Unit) = new CallbackAuthHandler(this, callback)
}

class NullAuthHandler[User](user: Option[User]) extends AuthHandler[User] {
  def authorized(ticket: Option[Array[Byte]]) = user
}

class CallbackAuthHandler[User](delegate: AuthHandler[User], callback: Option[User] => Unit) extends AuthHandler[User] {
  def authorized(ticket: Option[Array[Byte]]) = {
    val user = delegate.authorized(ticket)
    callback(user)
    user
  }
}

class ServerAuthHandler[User](auth: AuthHandler[User], users:java.util.Map[UUID,User], ldapUserLookup:BouncyLdapUserLookup[User],
                              userConnected:(User) => Unit, loggedIn: LoggedIn[User,UUID])
  extends SimpleChannelUpstreamHandler with AuthHandler[User] {

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) = {
    val channel = ctx.getChannel
    loggedIn.get(channel) match {
      case null => {
        e.getMessage match {
          case AuthMessage(ticket, overriddenUser) => {
            Logger.info("Server: User login message found")
            authorized(ticket) match {
              case Some(user) => {
                Logger.info("Server: User auth successful for " + user)
                userConnected(user)
                val userToUse = overriddenUser match {
                  case None => user
                  case Some(oUser) => {
                    Logger.info("Server: Overriding User to " + oUser)
                    ldapUserLookup.user(oUser) match {
                      case None => Logger.warn("Server: Couldn't override user to " + oUser + ", reverting to " + user);user
                      case Some(u) => u
                    }
                  }
                }
                Logger.info("Server: Actually using the user " + userToUse)
                val id = UUID.randomUUID()
                loggedIn.set(channel, userToUse, id)
                users.put(id, userToUse)
                channel.write(AuthSuccessfulMessage)
              }
              case None => {
                if(ticket != null && ticket.isDefined) {
                  Logger.info("Server: User auth failed, disconnecting client")
                } else {
                  Logger.info("Server: User auth failed, no kerberos ticket sent, disconnecting client")
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
            Logger.error("Unrecognised message on unauthorized channel, closing: " + m)
            channel.close
          }
        }
      }
      case user => {
        loggedIn.setLoggedOn(Some(user._1))
        ctx.sendUpstream(e)
        loggedIn.setLoggedOn(None)
      }
    }
  }

  def authorized(ticket: Option[Array[Byte]]) = auth.authorized(ticket)
}

