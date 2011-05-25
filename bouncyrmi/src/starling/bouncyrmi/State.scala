package starling.bouncyrmi

import starling.utils.{StackTraceToString, Log}

trait State {
  def move(event: Any): State = {
    try {
      val from = this
      val to = change(event)
      Log.info("State " + from + " -> " + to + " by " + event)
      to
    }
    catch {
      case e: Throwable => {
        e.printStackTrace
        throw new Exception("No state transition from " + this + " with event " + event)
      }
    }
  }

  protected def change(event: Any): State
}

// Initial state
class NotConnected extends State {
  def change(event: Any) = event match {
    case UnexpectedDisconnectEvent(t) => ConnectFailed(t)
    case ClientConnectedEvent => ClientConnecting
    case ClientDisconnectedEvent => ClientDisconnected
  }

  override def toString = "Not Connected State"
}

// Failed to connect. This can be a final state in the case of try to connect for the first time,
// or possibly a transition state if we are attempting to reconnect
case class ConnectFailed(t: Throwable) extends NotConnected {
  override def toString = "Connection Failed: " + StackTraceToString.string(t)
}

trait ConnectingState extends State {
  def change(event: Any) = event match {
  // Messages
    case VersionCheckResponse(version) if version == BouncyRMI.CodeVersion => {
      Log.info("Version check passed, same version: " + version)
      ClientConnected
    }
    case VersionCheckResponse(version) if version != BouncyRMI.CodeVersion => {
      Log.info("Version failed, server version: " + version + ", client version: " + BouncyRMI.CodeVersion)
      new ServerUpgrade(version)
    }
    case AuthFailedMessage => {
      Log.warn("Auth failed against server.")
      AuthFailed
    }

    // Events
    case ClientConnectedEvent => ClientConnecting
    case ClientDisconnectedEvent => ClientDisconnected
    case UnexpectedDisconnectEvent(t) => {
      t.printStackTrace
      Reconnecting(t)
    }
    case ShutdownMessage(msg) => ServerDisconnected(msg)
  }
}

case object ClientConnecting extends ConnectingState

case object ClientConnected extends State {
  def change(event: Any) = event match {
  // Messages
    case ShutdownMessage(msg) => ServerDisconnected(msg)
    case VersionCheckResponse(version) if version != BouncyRMI.CodeVersion => {
      Log.info("Version failed, server version: " + version + ", client version: " + BouncyRMI.CodeVersion)
      new ServerUpgrade(version)
    }

    // events
    case ClientDisconnectedEvent => ClientDisconnected
    case UnexpectedDisconnectEvent(t) => {
      t.printStackTrace
      Reconnecting(t)
    }
  }
}

case class Reconnecting(t: Throwable) extends ConnectingState
case class ServerDisconnected(msg: String) extends NotConnected {
  override def toString = "Server disconnected: " + msg
}

case object AuthFailed extends State {
  def change(event: Any) = event match {
    case _ => this
  }
}

case object ClientDisconnected extends State {
  def change(event: Any) = event match {
    case _ => this
  }
}

case class ServerUpgrade(serverVersion: String) extends State {
  def change(event: Any) = event match {
    case _ => this
  }
}
