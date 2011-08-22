package starling.bouncyrmi

import java.security.{KeyStore, Security}
import java.lang.String
import javax.net.ssl.{X509TrustManager, SSLContext, KeyManagerFactory}
import java.sql.Date
import java.security.cert.{CertificateException, X509Certificate}

object SslContextFactory {
  val algorithm = Security.getProperty("ssl.KeyManagerFactory.algorithm");

  def serverContext = {
    val ks = KeyStore.getInstance("JKS");
    val pass = "limetree".toCharArray
    val inputStream = resource("/cert.jks")
    assert(inputStream != null, "No cert")
    ks.load(inputStream, pass);

    // Set up key manager factory to use our key store
    val kmf = KeyManagerFactory.getInstance(algorithm)
    kmf.init(ks, pass)

    // Initialize the SSLContext to work with our key managers.
    val context = SSLContext.getInstance("TLS")
    context.init(kmf.getKeyManagers(), null, null)
    context
  }

  def clientContext = {
    val trustManager = new X509TrustManager() {
      def getAcceptedIssuers = Array()

      def checkServerTrusted(p1: Array[X509Certificate], p2: String) = {
        assert(p1.length == 1, "We expect 1 certificate: " + p1.length)
        val cert = p1(0)
        cert.checkValidity(new Date(System.currentTimeMillis))
        if (cert.getIssuerDN.getName != BouncyRMI.CertIssuerName) {
          throw new CertificateException("Invalid cert issuer:" + cert.getIssuerDN)
        }
      }

      // we'll never be here
      def checkClientTrusted(p1: Array[X509Certificate], p2: String) = {}
    }

    val context = SSLContext.getInstance("TLS")
    context.init(null, Array(trustManager), null)
    context
  }

  private def resource(resource: String) = {
    getClass.getResourceAsStream(resource)
  }
}