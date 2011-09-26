package starling.auth.internal

import javax.security.auth.Subject
import javax.security.auth.login.LoginContext
import ISVNSecurityLibrary.SEC_WINNT_AUTH_IDENTITY
import starling.utils.{Log, Utils}
import org.ietf.jgss.{GSSException, GSSContext, GSSName, GSSManager}
import java.security.{PrivilegedActionException, PrivilegedAction}
import sun.misc.BASE64Encoder
import java.io.{File, FileWriter}
import starling.auth.Client

object ClientLogin {

  def main(args: Array[String]) {
    println(Kerberos.krb5Oid)
    val subject = new ClientLogin().login
    val client = new RealClient("STARLING-TEST/dave-linux", subject)
    val ticket = client.ticket.get

    var encoder: BASE64Encoder = new BASE64Encoder
    var writer: FileWriter = new FileWriter(new File("ticket"))
    var encodedToken: String = encoder.encode(ticket)
    writer.write(encodedToken)
    writer.close
  }
}
class ClientLogin {
  Kerberos.init

  def login: Subject = {
    val loginCtx: LoginContext = new LoginContext("Client")
    loginCtx.login
    loginCtx.getSubject
  }
}

class RealClient(servicePrincipalName: String, subject: Subject) extends Client {

  /**
   * Returns a ticket that grants access to `servicePrincipalName`
   */
  def ticket: Option[Array[Byte]] = {
    Utils.os match {
      case _: Utils.Windows => ticketWindows
      case _ => ticketLinux
    }
  }

  def ticketWindows: Option[Array[Byte]] = {
    import com.sun.jna.platform.win32.{Advapi32Util, Sspi, Secur32, W32Errors}
    import com.sun.jna.ptr.NativeLongByReference
    import com.sun.jna.{WString, NativeLong}

    val authIdentity = new SEC_WINNT_AUTH_IDENTITY
    val user = Advapi32Util.getUserName
    Log.info("Starting SSPI for " + user)
    authIdentity.Domain = new WString(Kerberos.domain)
    authIdentity.DomainLength = new NativeLong(Kerberos.domain.length)
    authIdentity.Flags = new NativeLong(ISVNSecurityLibrary.SEC_WINNT_AUTH_IDENTITY_UNICODE)
    authIdentity.write

    val phClientCredential: Sspi.CredHandle = new Sspi.CredHandle

    val ptsClientExpiry: Sspi.TimeStamp = new Sspi.TimeStamp
    val handle: Int = Secur32.INSTANCE.AcquireCredentialsHandle(
      servicePrincipalName,
      "Negotiate",
      new NativeLong(Sspi.SECPKG_CRED_OUTBOUND),
      null,
      authIdentity.getPointer,
      null,
      null,
      phClientCredential,
      ptsClientExpiry)

    assert(handle == W32Errors.SEC_E_OK)

    val phClientContext: Sspi.CtxtHandle = new Sspi.CtxtHandle
    val pfClientContextAttr: NativeLongByReference = new NativeLongByReference

    var clientRc: Int = W32Errors.SEC_I_CONTINUE_NEEDED

    val pbClientToken: Sspi.SecBufferDesc = new Sspi.SecBufferDesc(Sspi.SECBUFFER_TOKEN, Sspi.MAX_TOKEN_SIZE)
    if (clientRc == W32Errors.SEC_I_CONTINUE_NEEDED) {
      val pbServerTokenCopy: Sspi.SecBufferDesc = null

      clientRc = Secur32.INSTANCE.InitializeSecurityContext(
        phClientCredential,
        if (phClientContext.isNull) null else phClientContext,
        servicePrincipalName,
        new NativeLong(Sspi.ISC_REQ_CONNECTION),
        new NativeLong(0),
        new NativeLong(0),
        pbServerTokenCopy,
        new NativeLong(0),
        phClientContext,
        pbClientToken,
        pfClientContextAttr,
        null)
    }
    // TODO [06 May 2011] After this call our state will still be SEC_I_CONTINUE_NEEDED.
    // That's because although the server may be happy with the authentication
    // the client needs more rounds. We don't care about the client trusting the
    // server so we can just finish here. I think...
    Log.info("Done SSPI for " + user + " with state: " + clientRc)

    Some(pbClientToken.getBytes)
  }


  def ticketLinux: Option[Array[Byte]] = {
    val manager: GSSManager = GSSManager.getInstance
    val serverName: GSSName = manager.createName(servicePrincipalName, GSSName.NT_USER_NAME)
    val context: GSSContext = manager.createContext(serverName, Kerberos.krb5Oid, null, GSSContext.DEFAULT_LIFETIME)
    try {
      Subject.doAs(subject, new PrivilegedAction[Option[Array[Byte]]] {
        def run = {
          try {
            val token: Array[Byte] = new Array[Byte](0)
            context.requestMutualAuth(false)
            context.requestCredDeleg(false)
            val secContext = context.initSecContext(token, 0, token.length)

            Some(secContext)
          }
          catch {
            case e: GSSException => {
              Log.error("GSS Exception codes: " + (e.getMajor, e.getMinor, e.getMinorString, e.getMajorString, e.getMessage))
              Log.error("Failed to get ticket", e)
              (e.getMajor, e.getMinorString) match {
                case (GSSException.NO_CRED, "KDC has no support for encryption type (14)") => {
                  throw new Exception("Failed to authorize, this looks like you're missing a kerberos registry key?", e)
                }
                case (GSSException.NO_CRED, "Integrity check on decrypted field failed (31)") => {
                  throw new Exception("Failed to authorize, this looks like you're missing a Windows 7 kerberos registry key?", e)
                }
                case (GSSException.NO_CRED, "Clock skew too great (37)") => {
                  // Clock skew too great
                  throw new Exception("Failed to authorize, your clock is possibly wrong, have a sys admin reset it.", e)
                }
                case (GSSException.OLD_TOKEN | GSSException.CONTEXT_EXPIRED | GSSException.CONTEXT_EXPIRED, _) => {
                  throw new Exception("Failed to authorize, your creditials have expired, try locking then unlocking your computer.", e)
                }
                case _ => throw new Exception(e.getMinorString + "(" + e.getMajor + "/" + e.getMinor + ")", e)
              }
            }
          }
        }
      })
    }
    catch {
      case e: PrivilegedActionException => throw e.getCause
    }
  }
}