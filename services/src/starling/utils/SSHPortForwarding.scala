package starling.utils

import java.net.{BindException, ServerSocket}
import com.jcraft.jsch.{KeyPair, JSch}

/**
 * Sets up ssh port forwarding and runs the block of code provided
 */

object SSHPortForwarding {

  def forward(
               userName:String,
               hostname:String,
               remotePort:Int,
               privateKeyFile:String)(action:(Int)=>Unit) {

    val jsch = new JSch();
    jsch.addIdentity(privateKeyFile)
    val session=jsch.getSession(userName, hostname, 22)
    session.setConfig("StrictHostKeyChecking", "no")
    val localPort = findFreePort()
    session.connect()

    session.setPortForwardingL(localPort, "localhost", remotePort)

    try {
      action(localPort)
    } finally {
      session.disconnect()
    }
  }

  private def findFreePort():Int = {
    var port = 8000
    while (port < 8050) {
      try {
        new ServerSocket(port).close()
        return port
      } catch {
        case e : BindException => false
      }
      port += 1
    }
    throw new Exception("Could not find free port between 8000 and 8050")
  }
}

object GenerateKey {
  def main(args:Array[String]) {
    val kpair=KeyPair.genKeyPair(new JSch(), KeyPair.DSA)
    //kpair.setPassphrase("x")
    kpair.writePrivateKey("key.private")
    kpair.writePublicKey("key.public", "comment")
    kpair.dispose()
    println("Done. See key.private and key.public")
  }
}
