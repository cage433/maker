package starling.bouncyrmi

import org.jboss.netty.channel._
import java.util.UUID
import starling.auth.{LdapUserLookup, User,AuthHandler}


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

class ServerAuthHandler(auth: AuthHandler, users:java.util.Map[UUID,User]) extends SimpleChannelUpstreamHandler {

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) = {
    val channel = ctx.getChannel
    ChannelLoggedIn.get(channel) match {
      case null => {
        e.getMessage match {
          case AuthMessage(ticket, overriddenUser) => {
            Logger.info("Server: User login message found")
            auth.authorized(ticket, overriddenUser) match {
              case Some(user) => {
                Logger.info("Server: User auth successful for " + user)
                //userConnected(user)
                val id = UUID.randomUUID()
                ChannelLoggedIn.set(channel, user, id)
                users.put(id, user)
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
        ChannelLoggedIn.setLoggedOn(Some(user._1))
        ctx.sendUpstream(e)
        ChannelLoggedIn.setLoggedOn(None)
      }
    }
  }
}

object ChannelLoggedIn {
  private val loggedIn = new ChannelLocal[(User,UUID)]()

  def get(channel: Channel) = loggedIn.get(channel)
  def set(channel: Channel, user: User, id:UUID) = loggedIn.set(channel, (user,id))
  def remove(channel: Channel) = loggedIn.remove(channel)
  def setLoggedOn(user: Option[User]) {
    User.setLoggedOn(user)
  }
}

