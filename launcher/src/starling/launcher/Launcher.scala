package starling.launcher

import starling.singleclasspathmanager.{SingleClasspathManager}
import java.net.{ConnectException, Socket, URL}
import starling.gui.osgi.{MetalsGuiBromptonActivator, GuiBromptonActivator}
import starling.manager.BromptonActivator
import starling.bouncyrmi.{GuiLaunchParameters, BouncyRMIClientBromptonActivator}
import starling.props.ServerTypeLabel
import scala.Predef._
import swing.Publisher
import java.io.{File, ByteArrayOutputStream, OutputStream}
import management.ManagementFactory
import starling.browser.osgi.{RunAsUser, BrowserBromptonActivator}
import starling.utils.{SingleClasspathBroadcasterActivator, StringIO}

//Starts the gui without osgi
object Launcher {
  def main(args: Array[String]) {
    if (args.length < 4) {
      throw new IllegalArgumentException("You need to specify 4 arguments: hostname, rmi port, servicePrincipalName and serverType")
    }
    val buffer = teeStdOut //If you print to stdout before this line, std will not be captured
    println("Launcher: Args: " + args.toList)
    println("Launcher: stdout setup " + ManagementFactory.getRuntimeMXBean.getUptime + "ms")

    val rmiHost = args(0)
    val rmiPort = args(1).toInt
    val servicePrincipalName = args(2)
    val serverType = ServerTypeLabel.fromName(args(3))

    if (args.length == 5) {
      try {
        val socket = new Socket("localhost", 7777)
        socket.close()
      } catch {
        case e:ConnectException => {
          start(buffer, rmiHost, rmiPort, servicePrincipalName, serverType)
        }
      }
      val st = args(4)
      val index = st.indexOf("://")
      val s = st.substring(index + 3)
      val url = new URL("http://localhost:7777/" + s)
      val stream = url.openStream()
      println("Launcher: triggered open page " + ManagementFactory.getRuntimeMXBean.getUptime + "ms")
      stream.close()
    } else {
      start(buffer, rmiHost, rmiPort, servicePrincipalName, serverType)
    }
  }

  def teeStdOut = {
    //I have found that this must be called before anything is written to std out otherwise it does't capture the events
    val publishingStream = new PublishingOutputStream
    System.setOut(new java.io.PrintStream(new TeeOutputStream(System.out, publishingStream)))
    System.setErr(new java.io.PrintStream(new TeeOutputStream(System.err, publishingStream)))
    //booter writes to a file, so by reading this file we pick up anything written to std before this call
    System.getProperty("stdout.logfile") match {
      case fileName:String => {
        val logsSoFar = StringIO.readBytesFromFile(new File(fileName))
        publishingStream.write(logsSoFar)
        publishingStream.write("============\n".getBytes)
      }
      case _ =>
    }
    publishingStream
  }

  /*invoked with reflection*/def startWithUser(overriddenUser:String) {
    if (rmiPort == -1) {
      throw new Exception("You can only run as user once start has been called")
    }
    start(None, rmiHost, rmiPort, servicePrincipalName, serverType.get, Some(overriddenUser))
  }

  // These variables are a big hack so we remember what they are when running the start method when changing users.
  private var rmiHost = ""
  private var rmiPort = -1
  private var servicePrincipalName = ""
  private var serverType : Option[ServerTypeLabel] = None
  private var stdOut : StdOut = _

  def start(stdOut:StdOut, rmiHost: String, rmiPort: Int, servicePrincipalName: String, serverType:ServerTypeLabel) {
    start(Some(stdOut), rmiHost, rmiPort, servicePrincipalName, serverType, None)
  }
  def start(maybeStdOut:Option[StdOut], rmiHost: String, rmiPort: Int, servicePrincipalName: String, serverType:ServerTypeLabel, overriddenUser:Option[String] = None) {
    this.rmiHost = rmiHost
    this.rmiPort = rmiPort
    this.servicePrincipalName = servicePrincipalName
    this.serverType = Some(serverType)
    maybeStdOut.foreach(b => this.stdOut = b)

    val launchParameters = GuiLaunchParameters(rmiHost, rmiPort, servicePrincipalName, overriddenUser)

    val baseActivators = List[Class[_ <: BromptonActivator]](
      classOf[SingleClasspathBroadcasterActivator],
      classOf[BrowserBromptonActivator],
      classOf[BouncyRMIClientBromptonActivator],
      classOf[GuiBromptonActivator],
      classOf[LauncherBromptonActivator]
    )
    val extraActivators = {
      import ServerTypeLabel._
      serverType match {
        case `FC2`|`Dev` => List[Class[_ <: BromptonActivator]](
          classOf[MetalsGuiBromptonActivator], classOf[JettyBromptonActivator])
        case `Oil` => List[Class[_ <: BromptonActivator]]()
      }
    }
    val activators = baseActivators ::: extraActivators
    val initialServices = List(
      classOf[GuiLaunchParameters] -> launchParameters,
      classOf[RunAsUser] -> RunAsUser(overriddenUser),
      classOf[StdOut] -> stdOut
    )
    val single = new SingleClasspathManager(true, activators, initialServices)
    single.start()
  }
}

class PublishingOutputStream extends OutputStream with StdOut {
  val buffer = new ByteArrayOutputStream()
  val publisher = new Publisher() {}

  def readAll = synchronized { buffer.toByteArray }

  private def broadcast(bytes:Array[Byte]) {
    publisher.publish(StdOutEvent(bytes))
  }
  def write(c: Int) {
    buffer.write(c)
    broadcast(Array[Byte](c.asInstanceOf[Byte]))
  }

  override def write(bytes: Array[Byte]) {
    buffer.write(bytes)
    broadcast(bytes)
  }

  override def write(bytes: Array[Byte], off: Int, len: Int) {
    buffer.write(bytes, off, len)
    broadcast(bytes.slice(off, off+len))
  }

}

class TeeOutputStream(a: OutputStream, b: OutputStream) extends OutputStream {

  def write(c: Int) {
    a.write(c)
    b.write(c)
  }

  override def write(bytes: Array[Byte]) {
    a.write(bytes)
    b.write(bytes)
  }

  override def write(bytes: Array[Byte], off: Int, len: Int) {
    a.write(bytes, off, len)
    b.write(bytes, off, len)
  }

  override def close() {
    a.close()
    b.close()
  }

  override def flush {
    a.flush
    b.flush
  }
}