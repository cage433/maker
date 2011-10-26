package starling.auth.internal

import java.util.Properties
import javax.security.auth.Subject
import javax.security.auth.login.LoginContext
import javax.security.auth.callback.{Callback, CallbackHandler, NameCallback, PasswordCallback}
import java.security.PrivilegedAction
import org.ietf.jgss.{GSSCredential, GSSManager}
import starling.utils.{StringIO, Log}
import sun.misc.BASE64Decoder
import java.io.File

private [internal] class ServerLogin(password: String) {
  Kerberos.init
  
  def login: Subject = {
    val loginCtx: LoginContext = new LoginContext("Server", new LoginCallbackHandler(password))
    loginCtx.login
    loginCtx.getSubject
  }
}

private [internal] object ServerLogin {
  def main(args: Array[String]) {
    println(Kerberos.krb5Oid)
    val subject = new ServerLogin("suvmerinWiv0").login
    val ticket = StringIO.readStringFromFile(new File("ticket"))
    val decoder: BASE64Decoder = new BASE64Decoder
    AuthenticationServer.verify(subject, decoder.decodeBuffer(ticket))
  }
}

private [internal] object AuthenticationServer {

  /**
   * Verifies the service ticket in the given subject.
   * Returns Some(username) or None if the ticket fails to verify 
   */
  def verify(subject: Subject, serviceTicket: Array[Byte]): Option[String] = {
    // Accept the context and return the client principal name.
    Subject.doAs(subject, new PrivilegedAction[Option[String]]() {
      def run = {
        try {
          // Identify the server that communications are being made to.
          val manager = GSSManager.getInstance
          val context = manager.createContext(null.asInstanceOf[GSSCredential])
          val arrayOfBytes = context.acceptSecContext(serviceTicket, 0, serviceTicket.length)
          assert(context.isEstablished, "Failed to estable context: " + context)
          val username = context.getSrcName.toString
          Log.info("Verified ticket for " + username)
          Some(username)
        }
        catch {
          case e => {
            Log.warn("Failed to verify", e)
            None
          }
        }
      }
    })
  }
}


private [internal] class LoginCallbackHandler(password: String) extends CallbackHandler {
  def handle(callbacks: Array[Callback]): Unit = {
    for (callback <- callbacks) callback match {
      case pc: PasswordCallback => pc.setPassword(password.toCharArray)
      case _ =>
    }
  }
}