package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import org.mortbay.util.IO
import scala.xml.XML
import scala.util.matching.Regex
import java.util.jar.{JarEntry, JarOutputStream}
import java.io._
import java.util.UUID
import starling.utils.cache.CacheFactory
import starling.props.PropsHelper
import starling.utils.ImplicitConversions._
import starling.utils.IOUtils
import starling.daterange.{Day, Timestamp}


object GUICode {

  val dirPrefix = ""//if (getClass.getClassLoader.toString.charAt(0).isDigit) "../" else ""
  val scalaLibraryJar = new File(dirPrefix + "lib/scala/lib_managed/scala-library-jar-2.9.1.jar")

  val classesPath = "/target/scala-2.9.1/classes/"
  val externalJarsPrefix = "library-"
  val moduleJarsPrefix = "module-"


  val fileCacheDir = new File("modulejarcache")
  if (!fileCacheDir.exists) fileCacheDir.mkdir()


  // The order of this list matters. It is the order things are attempted to be loaded so ensure it is optimised.
  val modules = List("daterange", "quantity", "utils", "auth", "bouncyrmi", "gui", "gui.api",
    "pivot", "pivot.utils", "browser", "browser.service", "fc2.facility", "launcher", "manager", "singleclasspathmanager",
    "reports.facility", "rabbit.event.viewer.api", "trade.facility", "osgimanager")

  val ivyDir = System.getProperty("user.home") + "/.ivy2/cache/"

  val libJarNames = Map(
    "scala-library" -> scalaLibraryJar,
    "scala-swing" -> new File("lib/scala/lib_managed/scala-swing-jar-2.9.1.jar"),
    "jna" -> new File(ivyDir + "sbt.bug.jna/jna/jars/jna-3.3.0.jar"),
    "platform" -> new File(ivyDir + "net.java.dev.jna/jna/jars/jna-3.3.0-platform.jar"),
    "cglib-nodep" -> new File(ivyDir + "cglib/cglib-nodep/jars/cglib-nodep-2.2.jar"),
    "commons-io" -> new File(ivyDir + "commons-io/commons-io/jars/commons-io-1.3.2.jar"),
    "netty" -> new File(ivyDir + "org.jboss.netty/netty/bundles/netty-3.2.5.Final.jar"),
    "jxlayer" -> new File(ivyDir + "jxlayer/jxlayer/jars/jxlayer-4.0.jar"),
    "looks" -> new File(ivyDir + "jgoodies/looks/jars/looks-2.3.1.jar"),
    "jfreechart" -> new File(ivyDir + "jfree/jfreechart/jars/jfreechart-1.0.0.jar"),
    "jcommon" -> new File(ivyDir + "jfree/jcommon/jars/jcommon-1.0.0.jar"),
    "servlet-api" -> new File(ivyDir + "javax.servlet/servlet-api/jars/servlet-api-2.5.jar"),
    "jetty" -> new File(ivyDir + "org.mortbay.jetty/jetty/jars/jetty-6.1.26.jar"),
    "jetty-utls" -> new File(ivyDir + "org.mortbay.jetty/jetty-util/jars/jetty-util-6.1.26.jar"),
    "browser-lib-miglayout-4.0-swing.jar" -> new File(ivyDir + "mig-swing/miglayout/jars/miglayout-4.0.jar"),
    "org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x" -> new File(ivyDir + "starling-external-jars/org.eclipse.mylyn.wikitext.core/jars/org.eclipse.mylyn.wikitext.core-1.4-e3x.jar"),
    "eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x" -> new File(ivyDir + "starling-external-jars/org.eclipse.mylyn.wikitext.textile.core/jars/org.eclipse.mylyn.wikitext.textile.core-1.4.jar"),
    "swingx-core" -> new File(ivyDir + "org.swinglabs/swingx-core/jars/swingx-core-1.6.2-2.jar"),
    "timingframework" -> new File(ivyDir + "net.java.dev.timingframework/timingframework/jars/timingframework-1.0.jar"),
    "commons-codec" -> new File(ivyDir + "commons-codec/commons-codec/jars/commons-codec-1.5.jar"),
    "google-collections" -> new File(ivyDir + "com.google.collections/google-collections/jars/google-collections-1.0.jar"),
    "joda-time" -> new File(ivyDir + "joda-time/joda-time/jars/joda-time-1.6.jar"),
    "log4j" -> new File(ivyDir + "log4j/log4j/jars/log4j-1.2.16.jar"),
    "slf4j-api" -> new File(ivyDir + "org.slf4j/slf4j-api/jars/slf4j-api-1.6.1.jar"),
    "slf4j-log4j12" -> new File(ivyDir + "org.slf4j/slf4j-log4j12/jars/slf4j-log4j12-1.6.1.jar"),
    "xstream" -> new File(ivyDir + "com.thoughtworks.xstream/xstream/jars/xstream-1.3.1.jar"),
    "memcached" -> new File(ivyDir + "spy/spymemcached/jars/spymemcached-2.7.3.jar"),
    "scalaz-core" -> new File(ivyDir + "org.scalaz/scalaz-core_2.9.1/jars/scalaz-core_2.9.1-6.0.3.jar"),
    "transloader" -> new File(ivyDir + "transloader/transloader/jars/transloader-0.4.jar")
  )

  def dependencies = {
    (modules.map { module => module -> md5ForModule(module) }, libJarNames.mapValues(file=>md5(file)))
  }

  def md5ForModule(module:String) = {
    val file = getOrGenerateModule(module)
    md5(file)
  }

  def lastModifiedForModule(module:String) = {
    val outputPath = new File(dirPrefix + module + "/target/scala-2.9.1/classes/")
    val resourcesPath = new File(dirPrefix + module + "/resources")
    val resourcesLastModified = if (resourcesPath.exists) findLastModified(resourcesPath) else 0
    findLastModified(
      outputPath,
      initialValue =  resourcesLastModified
    )
  }

  def findMatchingFiles(dir:File, matching:Regex = new Regex("^.*\\.(class|png|conf|auth|txt|properties)$")):List[File] = {
    (List[File]() /: dir.listFiles)((l:List[File], f:File) => {
      if (f.isDirectory) {
        l ++ findMatchingFiles(f, matching)
      } else if (f.isFile && matching.findFirstIn(f.getName).isDefined) {
        f :: l
      } else {
        l
      }
    })
  }

  def findLastModified(dir:File, matching:Regex = new Regex("^.*\\.(class|png|conf|auth|txt|properties)$"), initialValue:Long = 0):Long = {
    if (!dir.exists) throw new Exception(dir + " does not exist")
    (initialValue /: dir.listFiles)((lastModified:Long, f:File) => {
      if (f.isDirectory) {
        findLastModified(f, matching, lastModified)
      } else if (f.isFile && matching.findFirstIn(f.getName).isDefined) {
        math.max(lastModified, f.lastModified)
      } else {
        lastModified
      }
    })
  }
  
  val md5Cache = new java.util.concurrent.ConcurrentHashMap[File,(Long,String)]()
  def md5(file:File) = {
    val cached = md5Cache.get(file)
    if (cached == null || cached._1 != file.lastModified()) {
      val md5 = IOUtils.md5(file)
      md5Cache.put(file, (file.lastModified, md5))
      md5
    } else {
      cached._2
    }
  }

  val fileCache = CacheFactory.getCache("jarFileCache", soft = false)

  def memoize(file:String, f:()=>File) = fileCache.memoize(file, f())

  def getOrGenerateModule(module:String) = {
    val lastModified = GUICode.lastModifiedForModule(module)

    val moduleJarName = module + "_" + lastModified + ".jar"

    def getOrGenerateFile = {
      val moduleJarFile = new File(fileCacheDir, moduleJarName)
      if (moduleJarFile.exists) {
        moduleJarFile
      } else {
        val outputPath = new File(module + GUICode.classesPath)
        generateJar(moduleJarFile, outputPath)
        moduleJarFile.setLastModified(lastModified)
        moduleJarFile
      }
    }

    fileCache.memoize(moduleJarName, getOrGenerateFile)
  }

  def generateJar(jarFile:File, outputPath:File, main:Option[String] = None) {
    val jarOutputStream = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(jarFile)))
    if (outputPath.exists) {
      val files = GUICode.findMatchingFiles(outputPath).map(_.getPath)
      for (outputFile <- files) {
        jarOutputStream.putNextEntry(new JarEntry(outputFile.substring(outputPath.getPath.length+1)))
        val inputStream = new BufferedInputStream(new FileInputStream(outputFile))
        IO.copy(inputStream, jarOutputStream)
        inputStream.close
      }
    }
    main match {
      case Some(klass) => {
        jarOutputStream.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
        val manifest =
"""Manifest-Version: 1.0
Created-By: Starling
Main-Class: """ + klass + "\n"
        IO.copy(new ByteArrayInputStream(manifest.getBytes), jarOutputStream)
      }
      case None =>
    }
    jarOutputStream.flush
    jarOutputStream.close
  }
}

/**
 * A servlet for the web start / booter pages
 * Creates booter.jar which is a re-implementation of webstart
 * Also creates a jnlp file which servers up booter.jar for those machines where webstart works
 * booter.jar relies on app.txt which is similar to the jnlp file used by webstart
 */
class WebStartServlet(prefix:String, serverName:String, externalURL:String, mainClass:String, mainArguments:List[String],
                      xlloopUrl:String) extends HttpServlet {

  override def doGet(request: HttpServletRequest, response:HttpServletResponse) = {
    println("do GET " + request.getRequestURI)

    val excelPluginIniName = "excel_plugin-" + serverName + ".ini"
    val excelPluginXllName = "excel_plugin-" + serverName + ".xll"
    val booterName = "booter.jar"
    val starlingExeName = "Starling-" + serverName + ".exe"
    val starlingExeNoSplashName = "Starling-" + serverName + "-no-splash.exe"
    val kerberosName = "kerberos.reg"
    val starlingInstallerName = "Starling-" + serverName + "-InstallScript.nsi"
    val createInstallerName = "CreateInstallerInstructions.txt"

    var url = request.getRequestURI
    var path = url.substring(prefix.length + 1)
    if (path.length == 0 || path == "/") {
      response.setContentType("text/html")
      val rootPage = new WebPage(request) {
        def title = "Webstart: " + serverName
        def body = {
          <a href={link("launcher.jnlp")}>Webstart Launch</a><br/><br/>
                  <a href={link(booterName + "?" + UUID.randomUUID.toString)}>{booterName}</a><br/>
                  <a href={link(starlingExeName + "?" + UUID.randomUUID.toString)}>{starlingExeName}</a><br/>
                  <a href={link(starlingExeNoSplashName + "?" + UUID.randomUUID.toString)}>{starlingExeNoSplashName}</a><br/>
                  <a href={link(kerberosName)}>{kerberosName}</a><br/>
                  <a href={link(excelPluginIniName)}>{excelPluginIniName}</a><br/>
                  <a href={link(excelPluginXllName)}>{excelPluginXllName}</a><br/>
                  <a href={link(starlingInstallerName)}>{starlingInstallerName}</a><br/>
                  <a href={link(createInstallerName)}>{createInstallerName}</a><br/>
        }
      }
      response.setContentType("text/html")
      response.getWriter.println("""<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">""")
      XML.write(response.getWriter, rootPage.page, "UTF-8", true, null)
    } else if (path.equals("/launcher.jnlp")) {
        val jnlp = (
              <jnlp spec='1.0+' codebase={externalURL+"/webstart/"} href='launcher.jnlp'>
                <information>
                  <vendor>Trafigura</vendor>
                  <title>Starling: {serverName}</title>
                  <homepage href='http://starling'/>
                  <description>Starling: {serverName}</description>
                  <icon href='icon.png'/>
                  <offline-allowed/>
                  <shortcut><menu submenu="Trafigura"/></shortcut>
                </information>
                <security>
                    <all-permissions/>
                </security>
                <resources>
                  <!-- For people with USB monitors add:  java-vm-args="-Dsun.java2d.d3d=false"-->
                  <!-- with the options initial-heap-size='128m' max-heap-size='1024m'
                    Starling would not start on Scotts machine. There's just an error dialog:
                    'Could not create the java virtual machine' -->
                  <j2se version='1.6+' max-heap-size='812m' java-vm-args='-Dsun.java2d.d3d=false'/>
                  <jar href='booter.jar'/>
                </resources>
                <application-desc main-class="starling.booter.Booter">
                  <argument>{externalURL}</argument>
                  <argument>{serverName.replaceAll(" ", "_")}</argument>
                </application-desc>
                  <update check='always' policy='always'/>
              </jnlp>)
      response.setContentType("application/x-java-jnlp-file")
      XML.write(response.getWriter, jnlp, "UTF-8", true, null)
    } else if (path.equals("/app.txt")) {
      val writer =response.getWriter
      response.setContentType("text/plain")
      writer.println(mainClass + " " + PropsHelper.defaultProps.ExternalHostname() + " " + PropsHelper.defaultProps.RmiPort() + " " + PropsHelper.defaultProps.ServerPrincipalName() + " " + PropsHelper.defaultProps.ServerType().name)
      val (modules, libJarNames) = GUICode.dependencies
      for ((module, md5) <- modules) {writer.println(GUICode.moduleJarsPrefix + module + ".jar " + md5)}
      for ((libName, md5) <- libJarNames) {writer.println(GUICode.externalJarsPrefix + libName + ".jar " + md5)}
    } else if (path.startsWith("/" + GUICode.moduleJarsPrefix)) {
      println("Asking for module Jar: " + path + " " + request.getParameter("md5"))
      val module = {
        val p = path.replaceFirst("/" + GUICode.moduleJarsPrefix, "")
        p.substring(0, p.size-4)
      }
      val moduleJarFile =GUICode.getOrGenerateModule(module)
      writeFileToStream(moduleJarFile, response, Some(request.getParameter("md5")))
    } else if (path.startsWith("/" + GUICode.externalJarsPrefix)) {
      println("Asking for library Jar: " + path + " " + request.getParameter("md5"))
      val jarName = path.substring(1+GUICode.externalJarsPrefix.length)
      val jarFile = GUICode.libJarNames(jarName.substring(0, jarName.size-4))
      writeFileToStream(jarFile, response, Some(request.getParameter("md5")))
    } else if (path.equals("/icon.png")) {
      response.setContentType("image/png")
      IO.copy(classOf[WebStartServlet].getClassLoader.getResourceAsStream("icons/webstart.png"), response.getOutputStream)
    } else if (path == "/" + booterName) {
      println("Asking for booter.jar")
      val (booterJarFile, _) = generateBooterJar
      println("TS " + booterJarFile.lastModified())
      writeFileToStream(booterJarFile, response)
    } else if (path == "/" + starlingExeName) {
      val splashFile = new File("project/deployment/splash_screen.bmp")
      exeRequested(starlingExeName, Some(splashFile.getAbsolutePath))
    } else if (path == "/" + starlingExeNoSplashName) {
      exeRequested(starlingExeNoSplashName, None)
    } else if (path == "/" + kerberosName) {
      val kerberosFile = new File("project/deployment/kerberos.reg")
      writeFileToStream(kerberosFile, response)
    } else if (path == "/" + excelPluginIniName) {
      def generateFile = {
        val iniFile = new File(GUICode.fileCacheDir, excelPluginIniName + "-" + new Timestamp().toString)
        val bufferedWriter = new BufferedWriter(new FileWriter(iniFile))
        bufferedWriter.write("server=" + xlloopUrl + "\r\n") // I'm using windows carriage return + line feed on purpose here!
        bufferedWriter.write("send.user.info=true\r\n")
        bufferedWriter.close
        iniFile
      }
      val iniFile = GUICode.memoize(excelPluginIniName, generateFile _)
      writeFileToStream(iniFile, response)
    } else if (path == "/" + excelPluginXllName) {
      val xlloopFile = new File("project/deployment/xlloop-0.3.1.xll")
      writeFileToStream(xlloopFile, response)
    } else if (path == "/" + starlingInstallerName) {
      val installationFileName = starlingInstallerName
      def generateFile = {
        val installationFile = new File(GUICode.fileCacheDir, installationFileName + "-" + new Timestamp().toString)
        val text = InstallationHelper.generateNSISText(serverName)
        val bufferedWriter = new BufferedWriter(new FileWriter(installationFile))
        bufferedWriter.write(text)
        bufferedWriter.close()
        installationFile
      }
      val installationFile = GUICode.fileCache.memoize(installationFileName, generateFile)
      writeFileToStream(installationFile, response)
    } else if (path == "/" + createInstallerName) {
      val readMeFile = new File("project/deployment/InstallationCreator_READ_ME.txt")
      writeFileToStream(readMeFile, response)
    } else {
      response.sendError(404)
    }

    def generateBooterJar = {
      val classes = new File("booter/target/scala-2.9.1/classes")
      val timestamp = GUICode.findLastModified(classes)
      val booterJarName = "booter_" + timestamp + ".jar"

      def getOrGenerateBooterFile = {
        val booterJarFile = new File(GUICode.fileCacheDir, booterJarName)
        if (booterJarFile.exists) {
          booterJarFile
        } else {
          GUICode.generateJar(booterJarFile, classes, Some("starling.booter.Booter"))
          signJar(booterJarFile)
          booterJarFile.setLastModified(timestamp)
          booterJarFile
        }
      }
      (GUICode.memoize(booterJarName, getOrGenerateBooterFile _), timestamp)
    }

    def writeFileToStream(file:File, response:HttpServletResponse, maybeRequestedMD5:Option[String]=None) {
      def writeFile() {
        val inputStream = new BufferedInputStream(new FileInputStream(file))
        IO.copy(inputStream, response.getOutputStream)
        inputStream.close
        response.getOutputStream.flush()
        response.getOutputStream.close()
      }
      val actualMD5 = GUICode.md5(file)
      maybeRequestedMD5 match {
        case None => {
          response.setContentType("application/octet-stream")
          response.setDateHeader("Last-Modified", file.lastModified) //without this webstart will always use the old version
          writeFile()
        }
        case Some(requestedMD5) => {
          if (actualMD5 != requestedMD5) {
            response.setStatus(404)
            response.setContentType("text/plain")
            response.getWriter.write("Requested " + file.getName + " with md5 " + requestedMD5 + " but the current md5 is " + actualMD5)
          } else {
            response.setContentType("application/octet-stream")
            response.setDateHeader("Expires", Day(2020, 1, 1).millis)
            writeFile()
          }
        }
      }
    }

    def exeRequested(exeName0:String, splashPath:Option[String]) {
      val iconFile = new File("project/deployment/starling.ico")
      val (booterJarFile,booterTimestamp) = generateBooterJar
      val timestamp = math.max(booterTimestamp, new File("services" + GUICode.classesPath + "starling/http/InstallationHelper.class").lastModified())
      val classifier = splashPath.map(_ => "").getOrElse("-no-splash")
      val exeName = "Starling-" + serverName + "_" + timestamp + classifier + ".exe"

      def getOrGenerateFile = {
        val exeFile = new File(GUICode.fileCacheDir, exeName)
        if (exeFile.exists) {
          println("Returning the exe : " + exeName0)
          exeFile
        } else {
          println("Generating the exe : " + exeName0)
          val configFile = new File(GUICode.fileCacheDir, "launch4j.xml")
          val configXML = InstallationHelper.generateL4JXML(externalURL, serverName, booterJarFile.getAbsolutePath, iconFile.getAbsolutePath, splashPath, timestamp).toString
          val bufferedWriter = new BufferedWriter(new FileWriter(configFile))
          bufferedWriter.write(configXML)
          bufferedWriter.close
          println("Config: " + configFile.getAbsolutePath)
          execute("lib/launch4j/launch4j", configFile.getAbsolutePath)
          exeFile
        }
      }
      val exeFile = GUICode.memoize(exeName, getOrGenerateFile _)
      response.setHeader("Content-Disposition:", "attachment; filename="+exeName0 + "\"")
      writeFileToStream(exeFile, response)
    }
  }

  private def signJar(jarFile:File) = {
    if (!jarFile.exists) {
      throw new Exception("Can't sign jar as it does not exist: " + jarFile)
    }
    val jarSignerPath = new File(System.getProperty("java.home")).getParent + "/bin/jarsigner"
    println("JDK " + new File(System.getProperty("java.home")).getAbsolutePath)
    execute(jarSignerPath, "-keystore", "databases/webstart/starling.jks", "-storepass", "password", "-keypass", "password", jarFile.getPath, "mykey")
  }

  private def execute(args:String*) {
    val p = new ProcessBuilder(args :_*).start()
    val bufferedReader = new BufferedReader(new InputStreamReader(p.getInputStream))
    val errorBufferedReader = new BufferedReader(new InputStreamReader(p.getErrorStream))
    var line = ""
    var errorLine = ""
    var cont = true
    while (cont) {
      line = bufferedReader.readLine
      errorLine = errorBufferedReader.readLine
      if (line != null) {
        println("Output: " + line)
      }
      if (errorLine != null) {
        println("Error: " + errorLine)
      }
      if ((line == null) && (errorLine == null)) {
        cont = false
      }
    }
    bufferedReader.close
    errorBufferedReader.close
  }
}
