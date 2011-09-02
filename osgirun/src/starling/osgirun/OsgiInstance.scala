package starling.osgirun

import org.osgi.framework.launch.FrameworkFactory
import java.util.{HashMap, ServiceLoader}
import org.osgi.service.packageadmin.PackageAdmin
import java.net.{ServerSocket, ConnectException, Socket}
import java.util.concurrent.CountDownLatch

trait BundleDefinitions {
  def bundles:List[BundleDefinition]
  def systemPackages:List[String]
}

class OsgiInstance(name:String, bundles:BundleDefinitions) {
  //System.setProperty("log4j.configuration", "log4j.properties")
  System.setProperty("org.ops4j.pax.logging.DefaultServiceLog.level", "INFO")

  val factory:FrameworkFactory = ServiceLoader.load(classOf[FrameworkFactory], getClass.getClassLoader).iterator.next

  val frameworkProps = new HashMap[String, String]
  frameworkProps.put("org.osgi.framework.storage", name)
  frameworkProps.put("org.osgi.framework.bootdelegation", "sun.*,com.sun.*")
  frameworkProps.put("org.osgi.framework.system.packages.extra", bundles.systemPackages.mkString(","))

  val framework = factory.newFramework(frameworkProps)
  framework.init

  def update() {
    val context = framework.getBundleContext
    val allBundles = bundles.bundles.map(d => new LoggingBundleDefinition(d))

    val currentBundles = context.getBundles.map { bundle => bundle.getSymbolicName -> bundle }.toMap.filter(_._2.getBundleId != 0)
    val newBundles = allBundles.map { definition => definition.name.name -> definition }.toMap

    // uninstall, update, install, refresh & start.
    val uninstalled = (currentBundles.keySet -- newBundles.keySet).toList.map { bundleToRemove => currentBundles(bundleToRemove).uninstall(); currentBundles(bundleToRemove) }
    val x = (newBundles.keySet & currentBundles.keySet).toList.map { commonBundle => {
      val newBundleDef = newBundles(commonBundle)
      val currentBundle = currentBundles(commonBundle)
      if (newBundleDef.lastModified > currentBundle.getLastModified) {
        currentBundle.update(newBundleDef.inputStream)
        Left( currentBundle )
      } else {
        Right( currentBundle )
      }
    }}
    val updated = x.collect { case Left(b) => b}
    val unchanged = x.collect { case Right(b) => b}
    val installed = (newBundles.keySet -- currentBundles.keySet).toList.map { newBundleName => {
      val newBundleDef = newBundles(newBundleName)
      context.installBundle("from-bnd:" + newBundleDef.name, newBundleDef.inputStream)
    }}
    if (uninstalled.nonEmpty || updated.nonEmpty) {
      val packageAdmin = context.getService(context.getServiceReference(classOf[PackageAdmin].getName())).asInstanceOf[PackageAdmin]
      packageAdmin.refreshPackages(null)
    }
    installed.foreach(_.start())
  }

  def start = {
    update()
    val activators = bundles.bundles.map(b => if (b.activator) 1 else 0).sum
    val latch = new CountDownLatch(activators)
    framework.getBundleContext.registerService(classOf[CountDownLatch].getName, latch, null)
    framework.start
    latch.await()
  }
  def stop = {
    framework.stop
  }
}

object OsgiInstance {
  def startOrTrigger(name:String, bundles:BundleDefinitions) {
    val port = 1024 + ((name.hashCode.abs % 6400) * 10) + 9
    try {
      val socket = new Socket("localhost", port)
      socket.close
      println("triggered reload")
    } catch {
      case e:ConnectException => {
        val instance = new OsgiInstance(name, bundles)
        instance.start
        new Thread(new Runnable() { def run() {
          val server = new ServerSocket(port)
          while (true) {
            val client = server.accept()
            client.close()
            instance.update()
          }
        } }, "osgi-reload-listener").start()
      }
      //instance.stop
    }

  }
}
