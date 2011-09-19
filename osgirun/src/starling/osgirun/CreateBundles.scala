package starling.osgirun

import aQute.lib.osgi.Builder
import aQute.lib.osgi.Constants._
import util.matching.Regex
import collection.immutable.{Map, Iterable}
import org.apache.commons.io.{IOUtils, FileUtils}
import java.io._
import java.util.jar.{Attributes, JarFile}
import org.osgi.framework.Version
import scala.Serializable

case class BundleName(name:String, version:Version)

object BundleName {
  def apply(name:String, version:String):BundleName = {
    try {
      BundleName(name, Version.parseVersion(version))
    } catch {
      case e:Exception => {
        throw new Exception("Can't parse " + name + " bundle version " + version, e)
      }
    }
  }
  def fromJar(jarName:String) = {
    val noJar = jarName.substring(0, jarName.size-4)
    val hyphen = noJar.lastIndexOf("-")
    BundleName(noJar.substring(0, hyphen), Version.parseVersion(noJar.substring(hyphen+1)))
  }
}

trait BundleDefinition {
  def activator:Boolean
  def name:BundleName
  def lastModified:Long
  def inputStream:InputStream
}

class ExistingBundleDefinition(jarFile:File) extends BundleDefinition {
  def activator = false
  val manifest = new JarFile(jarFile).getManifest
  val symbolicName = manifest.getMainAttributes.getValue("Bundle-SymbolicName")
  val version = manifest.getMainAttributes.getValue("Bundle-Version")
  def name = BundleName(symbolicName, Version.parseVersion(version))
  def lastModified = jarFile.lastModified()
  def inputStream = new FileInputStream(jarFile)
}

class LoggingBundleDefinition(delegate:BundleDefinition) extends BundleDefinition {
  def activator = delegate.activator
  def name = delegate.name
  def lastModified = delegate.lastModified
  def inputStream = {
    println("Generating bundle " + name.name)
    delegate.inputStream
  }
  override def toString = delegate.toString
}

object FilesUtils {

	def allFiles(dir:File*):List[File] = {
    val dirs: List[File] = dir.toList
    dirs.flatMap { d =>
      if (!d.exists()) throw new Exception(d + " does not exist")
      d.listFiles.toList.flatMap { f => {
        if (f.isDirectory) allFiles(f) else List(f)
      } }
    }
	}
}

class Module(root:File) {
  def classesEndingWith(suffix:String) = {
    def pathFromBin(soFar:List[String], dir:File):List[String] = {
      if (dir.getName == "classes") soFar else {
        pathFromBin(dir.getName :: soFar, dir.getParentFile)
      }
    }
    val classes = classfiles.filter(f => f.getName.endsWith(suffix) && f.getName != suffix)
    classes.map { file =>
      val className = file.getName.substring(0, (file.getName.size -".class".size))
      val packageName = pathFromBin(Nil, file.getParentFile).mkString(".")
      packageName + "." + className
    }
  }
  def classfiles = FilesUtils.allFiles(classesDir)
  def classesDir = new File(root, "target/scala-2.9.1.final/classes")
  def resources = {
    val dir = new File(root, "resources")
    if (dir.exists) Some(dir) else None
  }
  def jars = {
    def jarsIn(dir:File) = if (!dir.exists) Nil else dir.listFiles.toList.filter(_.getName.endsWith(".jar"))
    (jarsIn(new File(root, "lib")) ::: jarsIn(new File(root, "lib_managed"))).filterNot(_.getName.contains("source")).filterNot(_.getName.contains("javadoc"))
  }
}

case class Bundle(name:String, workspace:File, bundleConfig:BundleConfig) {
  def internalJars = bundleConfig.internalJars
  def includes = bundleConfig.includes
  def excludes = bundleConfig.excludes
  def modules = bundleConfig.dirs.map(d => new Module(new File(workspace, d)))
  def bromptonActivator = classEndingWith(bundleConfig.activator, "BromptonActivator.class")
  def osgiActivator = classEndingWith(None, "OSGIActivator.class")
  def classEndingWith(specificModule:Option[String], suffix:String) = {
    val modulesToSearch = specificModule.map(m => List(new Module(new File(workspace, m)))).getOrElse(modules)
    modulesToSearch.flatMap(_.classesEndingWith(suffix)).toList match {
      case Nil => None
      case one :: Nil => Some(one)
      case many => throw new Exception("More than one " + suffix + " activator found " + many)
    }
  }
  def latestTimestamp = modules.flatMap(_.classfiles).map(_.lastModified) match {
    case Nil => 0
    case files => files.max
  }
  def classes = modules.map(_.classesDir)
  def resources = modules.flatMap(_.resources)
  def jars = modules.flatMap(_.jars)
}

case class BundleConfig(exportAll:Boolean=false, internalJars:List[String]=Nil, includes:List[String]=Nil, excludes:List[String]=Nil, activator:Option[String]=None, dirs:List[String])
class Manager(
               workspace:File,
               sharedJars:List[File],
               copyJars:Set[String],
               mergeJars:Map[(String,String),List[String]],
               ignoreJars:Set[String],
               bundles:Map[String,BundleConfig]) {
  val bundles_ = bundles.map { case (name, bundleConfig) => new Bundle(name, workspace, bundleConfig) }

  def versionName(filename:String) = {
    filename match {
      case "lift-json_2.9.0-jar-2.4-M2.jar" => ("lift-json", "2.4.0.M2")
      case "scala-swing-jar-2.9.1.jar" => ("scala-swing", "2.9.0")
      case "scala-library-jar-2.9.1.jar" => ("scala-library", "2.9.0")
      case "dispatch-json_2.8.0-0.7.4.jar" => ("dispatch-json", "0.7.4")
      case "scala-stm_2.9.1-jar-0.3.jar" => ("scala-stm", "0.0.3")
      case "scalaz-core_2.9.1-6.0.3.jar" => ("scalaz-core", "6.0.3")
      case "ojdbc6-11.2.0.1.0.jar" => ("ojdbc6", "11.2.0")
      case "scalacheck_2.9.1-jar-1.9.jar" => ("scalacheck", "1.9")
      //case "sbinary_2.8.1-0.4.0.jar" => ("sbinary", "0.4.0")
      case "jaxrs-api-1.2.GA.jar" => ("jaxrs-api", "1.2.0")
      case "resteasy-jaxrs-1.2.GA.jar" => ("resteasy-jaxrs", "1.2.0")
      //case "xml-apis-1.0.b2.jar" => ("xml-apis", "1.0.0.b2")
      case "sjson_2.8.0-jar-0.8.jar" => ("sjson", "0.8.0")
      case _ => {
        val separator = {
          filename.lastIndexOf("_") match {
            case -1 => filename.lastIndexOf("-")
            case o => o
          }
        }
        val nameNoJar = filename.substring(0, filename.size-4)
        if (separator != -1 && nameNoJar.charAt(separator+1).isDigit) {
          (nameNoJar.substring(0, separator), nameNoJar.substring(separator+1))
        } else {
          (nameNoJar, "0")
        }
      }
    }
  }

  def packageExcludes(filename:String) = {
    filename match {
      case "scala-model-with-persistence.jar" => List(
        "com.rabbitmq.messagepatterns.unicast", "com.trafigura.tradinghub.discovery",
        "org.hibernate.*", "com.trafigura.tradecapture.internal.refinedmetaldocumentservice")
      case "scala-hub-support-2.14.jar" => List("com.rabbitmq.messagepatterns.unicast", "org.apache.http.*")
      case "persistence-support-2.14.jar" => List("org.apache.http", "org.dom4j.*", "org.hibernate.*")

      case "titan-core.jar" => List("net.sf.ehcache.*", "oracle.ucp.*", "org.dbunit.*", "org.hibernate.*", "org.testng.*")
      case "titan-utils.jar" => List("net.sf.ehcache.*")
      case "titan-security.jar" => List("net.sf.ehcache.*", "org.apache.commons.lang.*",
        "org.springframework.ldap.core.*", "org.springframework.security.core.*",
        //org.springframework.context.support is used in one of the jars in   titan-hub-persistence-model
        "org.springframework.jmx.*",
        "org.springframework.context.support.*")

      case "oncrpc-1.0.5.jar" =>  List("org.acplt.oncrpc.web")
      case "sjson_2.8.0-jar-0.8.jar" => List("org.objenesis")
      case "cglib-nodep-jar-2.2.jar" => List("net.sf.cglib.asm.util", "org.apache.tools.*")
      case "netty-bundle-3.2.5.Final.jar" => List("org.jboss.logging", "com.google.protobuf", "javax.servlet.*")

      case "spring-core-jar-3.0.5.RELEASE.jar" => List("org.aspectj.*", "org.springframework.asm.*"/* "org.aspectjbridge", "org.aspectj.weaver"*/)
      case "spring-jdbc-jar-3.0.5.RELEASE.jar" => List("com.mchange.v2.c3p0", "com.sun.rowset", "org.apache.derby.*",
        "org.springframework.context.*", /*"org.springframework.dao.*", */"org.springframework.jndi.*")
      case "spring-tx-jar-3.0.5.RELEASE.jar" => List("com.ibm.wsspi.uow", "javax.ejb", "javax.resource.*",
        "org.aopalliance.*", "org.springframework.aop.*",
        "org.springframework.context.*", "org.springframework.jndi.*",
        "org.springframework.scheduling.*", "org.springframework.stereotype.*")
      //case "spring-context-3.0.5.RELEASE.jar" => List("bsh", "com.jamonapi", "com.sun.net.httpserver",
      //  "edu.emory.mathcs.backport.java.util.concurrent", "groovy.lang")
      //case "spring-expression-3.0.5.RELEASE.jar" => List("org.aspectj.bridge")
      case "spring-context-support-jar-3.0.5.RELEASE.jar" => List("commonj.*", "freemarker.*", "net.sf.ehcache.*",
        "net.sf.jasperreports.*", "org.apache.commons.collections.*", "org.apache.velocity.*", "org.quartz.*",
        "org.springframework.context.*", "org.springframework.jndi.*", "org.springframework.scheduling.*")
      case "spring-beans-jar-3.0.5.RELEASE.jar" => List("javax.el", "javax.inject")

      case "xlloop-0.3.1.jar" => List("org.apache.bsf", "org.boris.jxll", "org.jatha.*",
        "org.json", "org.mozilla.javascript", "se.rupy.http.*")
      case "paranamer-jar-2.3.jar" => List("javax.inject")
      case "scalatest-1.6.1.jar" => List("org.apache.tools.ant")
      case "resteasy-jaxrs-2.2.2.GA.jar" => List(
        "Acme.Serve", "javax.annotation.security", "org.apache.http.*", "org.junit.*", "org.scannotation.*")
      case "jtds-1.2.5.jar" => List("com.sun.net.ssl", "jcifs.*")
      case "ojdbc6-11.2.0.1.0.jar" => List(
        "com.sun.security.auth.module", "oracle.i18n.text.*",
        "oracle.security.pki.*", "oracle.xdb.*",
        "oracle.ons", "javax.resource.*"/*, "javax.resource.spi", "javax.resource.spi.endpoint"*/)
      case "xstream-jar-1.3.1.jar" => List("nu.xom", "org.dom4j.*", "org.jdom.*", "org.codehaus.jettison.*", "org.joda.time.*", "org.xmlpull.*") //?? just added org.codehaus.jettison for gui
      //case "scala-compiler.jar" => List("org.apache.tools.ant.*")
      //case "scalap-2.9.0.jar" => List("scala.tools.*")
      //case "javassist-3.6.0.GA.jar" => List("com.sun.jdi.*")
      case "log4j-bundle-1.2.16.jar" => List("com.ibm.uvm.tools")
      case "scannotation-1.0.2.jar" => List("javassist.bytecode.*")

      case "lift-json_2.9.0-jar-2.4-M2.jar" => List("scala.tools.scalap.*")

      //GUI jars

      case "looks-2.3.1.jar" => List("com.sun.java.swing.plaf.*")
      case "org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar" => List("org.apache.tools.ant.*","org.eclipse.core.*")
      case "jfreechart-jar-1.0.0.jar" => List("javax.servlet.*")
      case "transloader-0.4.jar" => List("COM.jrockit.reflect.*", "jrockit.vm.*")

      case none => Nil
    }
  }

  def definitions = {

    val allJars = (bundles_.flatMap(_.jars).toList ::: sharedJars).
      groupBy(_.getName).map { case (name, files) => { (name, files.iterator.next) } }

    val mergeJarValues = mergeJars.flatMap(_._2).toSet

    val internalJars = bundles_.flatMap(_.bundleConfig.internalJars).toList

    val skips = ignoreJars ++ copyJars ++ mergeJarValues ++ internalJars

    val jarsWithoutSkips = allJars.filterNot{ case (name, file) => skips.contains(file.getName)}

    val bundlesForJars = jarsWithoutSkips.map { case (name, file) => versionName(name) -> List(name) } ++ mergeJars

    val jarBundles = bundlesForJars.map { case ((bundleName, version), jars) => {
      val jarFiles = jars.map(allJars)
      new BundleDefinition {
        def name = BundleName(bundleName, version)
        def activator = false
        def lastModified = jarFiles.map(_.lastModified()).max
        def inputStream = {
          val allExcludes = jars.flatMap(packageExcludes).distinct

          val builder = new Builder()
          builder.setProperty( BUNDLE_SYMBOLICNAME, bundleName )
          if (version != "") {
            builder.setProperty( BUNDLE_VERSION, version )
          }
          val includes = (bundleName match {
            case "titan-hub-persistence-model" =>
              List("org.jboss.resteasy.client.core.marshallers", "org.jboss.resteasy.client.core")
            case "resteasy-jaxrs" => List("org.scannotation")
            case _ => Nil
          }) ::: List("net.sf.cglib.proxy", "net.sf.cglib.core", "net.sf.cglib.reflect")

          builder.setProperty( EXPORT_PACKAGE, "*")
          builder.setProperty( IMPORT_PACKAGE, (allExcludes.map(p => "!"+p) ::: includes ::: List("*")).mkString(","))
          jarFiles.foreach(builder.addClasspath)
          val jar = builder.build()
          println("Writing jar " + bundleName + " from (" + jarFiles + ") exludes: " + allExcludes)
          val out = new ByteArrayOutputStream()
          jar.write(out)
          new ByteArrayInputStream(out.toByteArray)
        }
        override def toString = jarFiles.map(_.toString).mkString(" & ")
      }
    }}

    val alreadyOsgiBundles = copyJars.map { jarName => new ExistingBundleDefinition(allJars(jarName)) }

    val starlingBundles = bundles_.map { bundle =>
      new BundleDefinition {
        private val bromptonActivator = bundle.bromptonActivator
        def name = BundleName(bundle.name, new Version(0, 0, 0))
        def lastModified = bundle.latestTimestamp
        def activator = {
          bromptonActivator.isDefined
        }
        def inputStream = {
          val builder = new Builder()

          bundle.resources.foreach(builder.addClasspath)
          bundle.classes.foreach(builder.addClasspath)
          bundle.bundleConfig.internalJars.foreach { jarName => {
            builder.addClasspath(allJars(jarName))
          }}

          val internalJars = bundle.internalJars.map(allJars)
          internalJars.foreach(builder.addClasspath)

          val jarExcludes = bundle.internalJars.flatMap { jarName => packageExcludes(jarName) }

          val allExcludes = (bundle.excludes ::: jarExcludes).toSet.toList
          var allIncludes = scala.collection.mutable.HashSet[String]()
          allIncludes ++= bundle.includes
          builder.setProperty( BUNDLE_SYMBOLICNAME, bundle.name )
          allIncludes += "net.sf.cglib.proxy"
          allIncludes += "net.sf.cglib.core"
          allIncludes += "net.sf.cglib.reflect"
          bromptonActivator.map { activator =>
            builder.setProperty( "Brompton-Activator", activator )
          }
          if (bundle.bundleConfig.exportAll) {
            builder.setProperty( EXPORT_PACKAGE, "*" )
          }
          //builder.setProperty(NOUSES, "true")
          //if (bundle.name == "main") {
          //  builder.setProperty( EXPORT_PACKAGE, "starling.services.osgi,starling.api.utils,starling.services,"+
          //    "starling.props,com.trafigura.services,com.trafigura.services.*")
          //}
          builder.setProperty( IMPORT_PACKAGE, (allExcludes.map(p => "!"+p) ::: allIncludes.toList ::: List("*")).mkString(","))
          bundle.osgiActivator.map { activator =>
            builder.setProperty( BUNDLE_ACTIVATOR, activator )
          }
          val jar = builder.build()
          val out = new ByteArrayOutputStream()
          jar.write(out)
          if (bundle.name == "main") jar.writeManifest(System.out)
          if (bundle.name == "utils") jar.writeManifest(System.out)
          if (bundle.name == "osgimanager") jar.writeManifest(System.out)
          if (bundle.name == "gui") jar.writeManifest(System.out)
          new ByteArrayInputStream(out.toByteArray)
        }
      }
    }
    starlingBundles.toList ::: alreadyOsgiBundles.toList ::: jarBundles.toList
  }
}