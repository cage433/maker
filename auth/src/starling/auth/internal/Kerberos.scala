package starling.auth.internal

import org.ietf.jgss.Oid
import starling.utils.StringIO._

private [internal] object Kerberos {
  val krb5Oid: Oid = new Oid("1.2.840.113554.1.2.2")
  val domain = "GLOBAL.TRAFIGURA.COM"

  def init {
//    System.setProperty("sun.security.krb5.debug", "true")
    System.setProperty("java.security.krb5.realm", "GLOBAL.TRAFIGURA.COM")
    System.setProperty("java.security.krb5.kdc", "londonad.global.trafigura.com")
    System.setProperty("java.security.krb5.conf", url(getClass, "/krb5.conf.auth").toExternalForm)
    System.setProperty("java.security.auth.login.config", url(getClass, "/jaas.conf.auth").toExternalForm)
    System.setProperty("javax.security.auth.useSubjectCredsOnly", "true")
  }
}