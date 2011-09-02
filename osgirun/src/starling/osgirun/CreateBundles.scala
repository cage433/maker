package starling.osgirun

import aQute.lib.osgi.Builder
import aQute.lib.osgi.Constants._
import util.matching.Regex
import collection.immutable.{Map, Iterable}
import org.apache.commons.io.{IOUtils, FileUtils}
import java.io._
import java.util.jar.{Attributes, JarFile}

case class BundleName(name:String, version:String)

object BundleName {
  def fromJar(jarName:String) = {
    val noJar = jarName.substring(0, jarName.size-4)
    val hyphen = noJar.lastIndexOf("-")
    BundleName(noJar.substring(0, hyphen), noJar.substring(hyphen+1))
  }
}

trait BundleDefinition {
  def name:BundleName
  def lastModified:Long
  def inputStream:InputStream
}

class ExistingBundleDefinition(jarFile:File) extends BundleDefinition {
  val manifest = new JarFile(jarFile).getManifest
  val symbolicName = manifest.getMainAttributes.getValue("Bundle-SymbolicName")
  val version = manifest.getMainAttributes.getValue("Bundle-Version")
  def name = BundleName(symbolicName, version)
  def lastModified = jarFile.lastModified()
  def inputStream = new FileInputStream(jarFile)
}

class LoggingBundleDefinition(deligate:BundleDefinition) extends BundleDefinition {
  def name = deligate.name
  def lastModified = deligate.lastModified
  def inputStream = {
    println("Generating bundle " + name.name)
    deligate.inputStream
  }
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
  def classesDir = new File(root, "target/scala-2.9.0.1/classes")
  def resources = {
    val dir = new File(root, "resources")
    if (dir.exists) Some(dir) else None
  }
  def jars = {
    def jarsIn(dir:File) = if (!dir.exists) Nil else dir.listFiles.toList.filter(_.getName.endsWith(".jar"))
    jarsIn(new File(root, "lib")).filterNot(_.getName.endsWith("sources.jar"))
  }
}

case class Bundle(name:String, workspace:File, exportAll:Boolean, internalJars:List[String], includes:List[String], excludes:List[String], dirs:List[String]) {
  def modules = dirs.map(d => new Module(new File(workspace, d)))
  def bromptonActivator = classEndingWith("BromptonActivator.class")
  def osgiActivator = classEndingWith("OSGIActivator.class")
  def classEndingWith(suffix:String) = {
    modules.flatMap(_.classesEndingWith(suffix)).toList match {
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
    /*(if (name == "main") List(new File(workspace, ".ivy/cache/joda-time/joda-time/jars/joda-time-1.6.jar")) else Nil*/Nil
  def resources = modules.flatMap(_.resources)
  def jars = modules.flatMap(_.jars)
}

class Manager(
               workspace:File,
               sharedJars:List[File],
               copyJars:Set[String],
               mergeJars:Map[(String,String),List[String]],
               ignoreJars:Set[String],
               bundles:Map[String,(Boolean, List[String], (List[String], List[String]), List[String])]) {
  val bundles_ = bundles.map { case (name, (exportAll, internalJars, (includes, excludes), dirs)) => new Bundle(name, workspace, exportAll, internalJars, includes, excludes, dirs) }

  def versionName(filename:String) = {
    filename match {
//            case "jminx-json-lib-2.2.3-jdk15.jar" => ("jminx-json-lib","2.2.3")
      case "lift-json_2.9.0-2.4-M2.jar" => ("lift-json", "2.4.0.M2")
      case "scala-swing.jar" => ("scala-swing", "2.9.0")
      case "scala-library.jar" => ("scala-library", "2.9.0")
      case "dispatch-json_2.8.0-0.7.4.jar" => ("dispatch-json", "0.7.4")
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
        "org.hibernate.*")
      case "scala-hub-support-2.14.jar" => List("com.rabbitmq.messagepatterns.unicast", "org.apache.http.*")
      case "persistence-support-2.14.jar" => List("org.apache.http", "org.dom4j.*", "org.hibernate.*")

      case "titan-core.jar" => List("net.sf.ehcache.*", "oracle.ucp.*", "org.dbunit.*", "org.hibernate.*", "org.testng.*")
      case "titan-utils.jar" => List("net.sf.ehcache.*")
      case "titan-security.jar" => List("net.sf.ehcache.*", "org.apache.commons.lang.*",
        "org.springframework.ldap.core.*", "org.springframework.security.core.*")

      case "oncrpc-1.0.5.jar" =>  List("org.acplt.oncrpc.web")
      case "sjson_2.8.0-0.8.jar" => List("org.objenesis")
      case "cglib-nodep-2.2.jar" => List("net.sf.cglib.asm.util", "org.apache.tools.*")
      case "netty-3.2.5.Final.jar" => List("org.jboss.logging", "com.google.protobuf", "javax.servlet.*")

      case "spring-aop-3.0.5.RELEASE.jar" => List("com.jamonapi", "org.apache.commons.pool")
      case "spring-core-3.0.5.RELEASE.jar" => List("org.aspectj.*"/* "org.aspectjbridge", "org.aspectj.weaver"*/)
      case "spring-jdbc-3.0.5.RELEASE.jar" => List("com.mchange.v2.c3p0", "com.sun.rowset", "org.apache.derby.*")
      case "spring-context-support-3.0.5.RELEASE.jar" => List("commonj.timers", "commonj.work", "freemarker.*", "net.sf.ehcache")
      case "spring-tx-3.0.5.RELEASE.jar" => List("com.ibm.wsspi.uow", "javax.ejb", "javax.resource")
      case "spring-beans-3.0.5.RELEASE.jar" => List("javax.el", "javax.inject")
      case "spring-context-3.0.5.RELEASE.jar" => List("bsh", "com.jamonapi", "com.sun.net.httpserver",
        "edu.emory.mathcs.backport.java.util.concurrent", "groovy.lang")
      //case "spring-expression-3.0.5.RELEASE.jar" => List("org.aspectj.bridge")

      case "xlloop-0.3.1.jar" => List("org.apache.bsf", "org.boris.jxll", "org.jatha.*",
        "org.json", "org.mozilla.javascript", "se.rupy.http.*")
      case "paranamer-2.3.jar" => List("javax.inject")
      case "scalatest-1.6.1.jar" => List("org.apache.tools.ant")
      case "resteasy-jaxrs-1.2.GA.jar" | "resteasy-jaxrs-2.2.2.GA.jar" => List(
        "Acme.Serve", "javax.annotation.security", "org.apache.http.*", "org.junit.*", "org.scannotation.*")
      case "jtds-1.2.5.jar" => List("com.sun.net.ssl", "jcifs.*")
      case "ojdbc6-11.2.0.1.0.jar" => List(
        "com.sun.security.auth.module", "oracle.i18n.text.*",
        "oracle.security.pki.*", "oracle.xdb.*",
        "oracle.ons", "javax.resource.*"/*, "javax.resource.spi", "javax.resource.spi.endpoint"*/)
      case "xstream-1.3.1.jar" => List("nu.xom", "org.dom4j.*", "org.jdom.*", "org.codehaus.jettison.*", "org.joda.time.*", "org.xmlpull.*") //?? just added org.codehaus.jettison for gui
      //case "scala-compiler.jar" => List("org.apache.tools.ant.*")
      case "scalap-2.9.0.jar" => List("scala.tools.*")
      case "javassist-3.6.0.GA.jar" => List("com.sun.jdi.*")
      case "log4j-1.2.16.jar" => List("com.ibm.uvm.tools")

      //GUI jars

      case "looks-2.3.1.jar" => List("com.sun.java.swing.plaf.*")
      case "org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar" => List("org.apache.tools.ant.*","org.eclipse.core.*")
      case "jfreechart-1.0.0.jar" => List("javax.servlet.*")
      case "transloader-0.4.jar" => List("COM.jrockit.reflect.*", "jrockit.vm.*")

      case _ => Nil
    }
  }

  def definitions = {

    val allJars = (bundles_.flatMap(_.jars).toList ::: sharedJars).
      groupBy(_.getName).map { case (name, files) => { (name, files.iterator.next) } }

    val mergeJarValues = mergeJars.flatMap(_._2).toSet

    val internalJars = bundles_.flatMap(_.internalJars).toList

    val skips = ignoreJars ++ copyJars ++ mergeJarValues ++ internalJars

    val bundlesForJars = {
      allJars.filterNot{case (name, file) => skips.contains(name)}.
        map { case (name, file) => versionName(name) -> List(name) } ++
      mergeJars
    }

    val jarBundles = bundlesForJars.map { case ((bundleName, version), jars) => {
      val jarFiles = jars.map(allJars)
      new BundleDefinition {
        def name = BundleName(bundleName, version)
        def lastModified = jarFiles.map(_.lastModified()).max
        def inputStream = {
          val allExcludes = jars.flatMap(packageExcludes).distinct

          val builder = new Builder()
          builder.setProperty( BUNDLE_SYMBOLICNAME, bundleName )
          if (version != "") {
            builder.setProperty( BUNDLE_VERSION, version )
          }
          val includes = bundleName match {
            case "titan-hub-persistence-model" =>
              List("org.jboss.resteasy.client.core.marshallers", "org.jboss.resteasy.client.core")
            case "resteasy-jaxrs" => List("org.scannotation")
            case _ => Nil
          }
          builder.setProperty( EXPORT_PACKAGE, "*")
          builder.setProperty( IMPORT_PACKAGE, (allExcludes.map(p => "!"+p) ::: includes ::: List("*")).mkString(","))
          jarFiles.foreach(builder.addClasspath)
          val jar = builder.build()
          println("Writing jar " + bundleName + " from (" + jarFiles + ") exludes: " + allExcludes)
          val out = new ByteArrayOutputStream()
          jar.write(out)
          new ByteArrayInputStream(out.toByteArray)
        }
      }
    }}

    val alreadyOsgiBundles = copyJars.map { jarName => new ExistingBundleDefinition(allJars(jarName)) }

    val starlingBundles = bundles_.map { bundle =>
      new BundleDefinition {
        def name = BundleName(bundle.name, "0")
        def lastModified = bundle.latestTimestamp
        def inputStream = {
          val builder = new Builder()

          bundle.resources.foreach(builder.addClasspath)
          bundle.classes.foreach(builder.addClasspath)

          val internalJars = bundle.internalJars.map(allJars)
          internalJars.foreach(builder.addClasspath)

          val jarExcludes = bundle.internalJars.flatMap { jarName => packageExcludes(jarName) }

          val allExcludes = (bundle.excludes ::: jarExcludes).toSet.toList
          var allIncludes = scala.collection.mutable.HashSet[String]()
          allIncludes ++= bundle.includes
          builder.setProperty( BUNDLE_SYMBOLICNAME, bundle.name )
          bundle.bromptonActivator.map { activator =>
            allIncludes += "net.sf.cglib.proxy"
            allIncludes += "net.sf.cglib.core"
            builder.setProperty( "Brompton-Activator", activator )
          }
          builder.setProperty( EXPORT_PACKAGE, "*" )
          builder.setProperty( IMPORT_PACKAGE, (allExcludes.map(p => "!"+p) ::: allIncludes.toList ::: List("*")).mkString(","))
          bundle.osgiActivator.map { activator =>
            builder.setProperty( BUNDLE_ACTIVATOR, activator )
          }
          val jar = builder.build()
          val out = new ByteArrayOutputStream()
          jar.write(out)
          if (bundle.name == "main") jar.writeManifest(System.out)
          new ByteArrayInputStream(out.toByteArray)
        }
      }
    }
    starlingBundles.toList ::: alreadyOsgiBundles.toList ::: jarBundles.toList
  }
}