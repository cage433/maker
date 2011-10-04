package starling.osgirun

import org.osgi.framework.launch.FrameworkFactory
import org.osgi.service.packageadmin.PackageAdmin
import java.net.{ServerSocket, ConnectException, Socket}
import java.util.concurrent.CountDownLatch
import collection.immutable.Map
import org.osgi.util.tracker.ServiceTracker
import org.osgi.framework.{BundleContext, ServiceReference, FrameworkEvent, FrameworkListener}
import java.util.{Hashtable, Dictionary, HashMap, ServiceLoader}
import management.ManagementFactory
import java.io.File

trait BundleDefinitions {
  def bundles:List[BundleDefinition]
  def systemPackages:List[String]
}

class OsgiInstance(name:String, writePID:Boolean, properties:()=>Map[String,String], bundles:BundleDefinitions) {
  //System.setProperty("log4j.configuration", "log4j.properties")
  System.setProperty("org.ops4j.pax.logging.DefaultServiceLog.level", "INFO")

  val factory:FrameworkFactory = ServiceLoader.load(classOf[FrameworkFactory], getClass.getClassLoader).iterator.next

  val frameworkProps = new HashMap[String, String]
  frameworkProps.put("org.osgi.framework.storage", name)
  frameworkProps.put("org.osgi.framework.bootdelegation", "sun.*,com.sun.*")
  frameworkProps.put("org.osgi.framework.system.packages.extra", bundles.systemPackages.mkString(","))

  val framework = factory.newFramework(frameworkProps)
  framework.init

  framework.getBundleContext.addFrameworkListener(new FrameworkListener() {
    def frameworkEvent(event:FrameworkEvent) {
      println("Event " + event)
      if (event.getThrowable != null) {
        event.getThrowable.printStackTrace()
      }
    }
  })


  def update() {
    val context = framework.getBundleContext
    val allBundles = bundles.bundles.map(d => new LoggingBundleDefinition(d))



    val currentBundles = context.getBundles.map { bundle => BundleName(bundle.getSymbolicName, bundle.getVersion) -> bundle }.toMap.filter(_._2.getBundleId != 0)

    val newBundles = allBundles.map { definition => definition.name -> definition }.toMap


    val currentByName = context.getBundles.groupBy(_.getSymbolicName)
    val newByName = allBundles.groupBy(_.name.name)

    (currentByName.keySet & newByName.keySet).toList.map { name => {
      val versions1 = currentByName(name).map(_.getVersion).toSet
      val versions2 = newByName(name).map(_.name.version).toSet
      if (versions1 != versions2) {
        println(name + " => " + versions1 + " !+ " + versions2)
      }
    }}


    val ignoredBundles: Map[String, List[LoggingBundleDefinition]] =
      allBundles.groupBy(_.name.name).filter(_._2.size > 1).mapValues(_.init)

    println("ignored bundles: " + ignoredBundles.flatMap(_._2.map(_.name)).mkString(", "))

    // uninstall, update, install, refresh & start.
    val uninstalled = (currentBundles.keySet -- newBundles.keySet).toList.map { bundleToRemove => currentBundles(bundleToRemove).uninstall(); currentBundles(bundleToRemove) }

    println("uninstalled bundles: " + uninstalled.map(_.getSymbolicName).mkString(", "))

    val updated = (newBundles.keySet & currentBundles.keySet).toList.flatMap { commonBundle => {
      val newBundleDef = newBundles(commonBundle)
      val currentBundle = currentBundles(commonBundle)
      if (newBundleDef.lastModified > currentBundle.getLastModified|| newBundleDef.name.name == "auth") {
        println("updating: " + currentBundle.getSymbolicName + "...")
        currentBundle.update(newBundleDef.inputStream)
        println("updated: %s (state: %s)".format(currentBundle.getSymbolicName, currentBundle.getState))
        Some( currentBundle )
      } else {
        None
      }
    }}

    println("updated bundles: " + updated.map(_.getSymbolicName).mkString(", "))

    val installed = (newBundles.keySet -- currentBundles.keySet).toList.map { newBundleName => {
      println("installing: " + newBundleName + "...")
      val newBundleDef = newBundles(newBundleName)
      val res = context.installBundle("from-bnd:" + newBundleDef.name, newBundleDef.inputStream)
      println("installed: %s (state: %s)".format(newBundleName, res.getState))
      res
    }}

    println("installed bundles: " + installed.map(_.getSymbolicName).mkString(", "))

    if (uninstalled.nonEmpty || updated.nonEmpty) {
      val packageAdmin = context.getService(context.getServiceReference(classOf[PackageAdmin].getName())).asInstanceOf[PackageAdmin]
      packageAdmin.refreshPackages(null)
    }
    installed.foreach(_.start())
  }

  def writePIDFile() {
    val processName = ManagementFactory.getRuntimeMXBean.getName
    val pid = processName.subSequence(0, processName.indexOf("@")).toString

    val file = new File("pid.txt")
    if (file.exists) file.delete
    val out = new java.io.FileWriter(file)
    out.write(pid + "\n")
    out.close

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() = if (file.exists) file.delete
    })
  }


  def start = {
    if (writePID) { writePIDFile() }
    update()
    val bundlesWithActivators = bundles.bundles.filter(_.activator).map(_.name)
    val latch = new CountDownLatch(bundlesWithActivators.size)
    val bundleContext: BundleContext = framework.getBundleContext
    bundleContext.registerService(classOf[CountDownLatch].getName, latch, null)
    framework.start

    val tracker:ServiceTracker = new ServiceTracker(bundleContext, "starling.osgimanager.PropsService", null) {
      override def addingService(reference: ServiceReference) = {
        val propsService = bundleContext.getService(reference)
        val propertiesAsDictionary = mapToDictionary(properties())
        propsService.getClass.getMethod("updated", classOf[Dictionary[_,_]]).invoke(propsService, propertiesAsDictionary)
        null
      }
    }
    tracker.open()

    latch.await()
  }
  def stop = {
    framework.stop
  }

  def mapToDictionary(map:Map[String,AnyRef]):Dictionary[_,_] = {
    val table = new Hashtable[String,AnyRef]()
    map.foreach ( kv => table.put(kv._1, kv._2))
    table
  }

}

object OsgiInstance {
  def startOrTrigger(name:String, writePID:Boolean, properties:()=>Map[String,String], bundles:BundleDefinitions) {
    val port = 1024 + ((name.hashCode.abs % 6400) * 10) + 9
    try {
      val socket = new Socket("localhost", port)
      socket.close
      println("triggered reload")
    } catch {
      case e:ConnectException => {
        val instance = new OsgiInstance(name, writePID, properties, bundles)
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
