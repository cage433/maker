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
import starling.daterange.Timestamp


object GUICode {

  val dirPrefix = ""//if (getClass.getClassLoader.toString.charAt(0).isDigit) "../" else ""
  val scalaLibraryJar = new File(dirPrefix + "lib/scala/lib_managed/scala-library-jar-2.9.1.jar")

  // The order of this list matters. It is the order things are attempted to be loaded so ensure it is optimised.
  val modules = List("daterange", "quantity", "utils", "auth", "bouncyrmi", "gui", "gui.api",
    "pivot", "pivot.utils", "browser", "browser.service", "fc2.facility", "launcher", "manager", "singleclasspathmanager",
    "reports.facility", "rabbit.event.viewer.api", "trade.facility", "osgimanager")

  val libJarNames = Map(
    "scala-library" -> scalaLibraryJar,
    "scala-swing" -> new File("lib/scala/lib_managed/scala-swing-jar-2.9.1.jar"),
    "jna" -> new File("auth/lib/jna.jar"),
    "platform" -> new File("auth/lib/platform.jar"),
    "cglib-nodep" -> new File("bouncyrmi/lib_managed/cglib-nodep-jar-2.2.jar"),
    "commons-io" -> new File("bouncyrmi/lib_managed/commons-io-jar-1.3.2.jar"),
    "netty" -> new File("bouncyrmi/lib_managed/netty-bundle-3.2.5.Final.jar"),
    "jxlayer" -> new File("browser/lib/jxlayer-4.0.jar"),
    "looks" -> new File("browser/lib/looks-2.3.1.jar"),
    "jfreechart" -> new File("gui/lib_managed/jfreechart-jar-1.0.0.jar"),
    "servlet-api" -> new File("lib/servlet-api-jar-2.5.jar"),
    "jetty" -> new File("services/lib_managed/jetty-jar-6.1.26.jar"),
    "jetty-utls" -> new File("services/lib_managed/jetty-util-jar-6.1.26.jar"),
    "browser-lib-miglayout-4.0-swing.jar" -> new File("browser/lib/miglayout-4.0-swing.jar"),
    "org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x" -> new File("browser/lib/org.eclipse.mylyn.wikitext.core_1.4.0.I20100805-0500-e3x.jar"),
    "eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x" -> new File("browser/lib/org.eclipse.mylyn.wikitext.textile.core_1.4.0.I20100805-0500-e3x.jar"),
    "swingx-core" -> new File("browser/lib/swingx-core-1.6.2.jar"),
    "timingframework" -> new File("browser/lib/timingframework-1.0.jar"),
    "commons-codec" -> new File("utils/lib_managed/commons-codec-jar-1.4.jar"),
    "google-collections" -> new File("utils/lib_managed/google-collections-jar-1.0.jar"),
    "joda-time" -> new File("utils/lib_managed/joda-time-jar-1.6.jar"),
    "log4j" -> new File("utils/lib_managed/log4j-bundle-1.2.16.jar"),
    "slf4j-api" -> new File("databases/lib_managed/slf4j-api-jar-1.6.1.jar"),
    "slf4j-log4j12" -> new File("utils/lib_managed/slf4j-log4j12-jar-1.6.1.jar"),
    "xstream" -> new File("utils/lib_managed/xstream-jar-1.3.1.jar"),
    "memcached" -> new File("utils/lib/memcached-2.5.jar"),
    "scalaz-core" -> new File("utils/lib/scalaz-core_2.9.1-6.0.3.jar"),
    "transloader" -> new File("browser/lib/transloader-0.4.jar")
  )

  def dependencies = {
    (modules.map { module => module -> lastModifiedForModule(module) }, libJarNames.mapValues(file=>file.lastModified))
  }

  def latestTimestamp : Long = {
    val (modules, libraries) = dependencies
    (modules ++ libraries).maximum(_._2)
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
  
}

/**
 * A servlet for the web start / booter pages
 * Creates booter.jar which is a re-implementation of webstart
 * Also creates a jnlp file which servers up booter.jar for those machines where webstart works
 * booter.jar relies on app.txt which is similar to the jnlp file used by webstart
 */
class WebStartServlet(prefix:String, serverName:String, externalURL:String, mainClass:String, mainArguments:List[String],
                      xlloopUrl:String) extends HttpServlet {

  val externalJarsPrefix = "library-"
  val moduleJarsPrefix = "module-"


  val fileCacheDir = new File("modulejarcache")
  if (!fileCacheDir.exists) fileCacheDir.mkdir()

  val fileCache = CacheFactory.getCache("jarFileCache", soft = false)

  override def doGet(request: HttpServletRequest, response:HttpServletResponse) = {
    println("do GET")

    val excelPluginIniName = "excel_plugin-" + serverName + ".ini"
    val excelPluginXllName = "excel_plugin-" + serverName + ".xll"
    val booterName = "booter.jar"
    val starlingExeName = "Starling-" + serverName + ".exe"
    val kerberosName = "kerberos.reg"
    val starlingInstallerName = "Starling-" + serverName + "-InstallScript.nsi"
    val createInstallerName = "CreateInstallerInstructions.txt"
    val classesPath = "/target/scala-2.9.1/classes/"

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
      writer.println(mainClass + " " + PropsHelper.defaultProps.ExternalHostname() + " " + PropsHelper.defaultProps.RmiPort() + " " + PropsHelper.defaultProps.ServerPrincipalName() + " " + PropsHelper.defaultProps.ServerType())
      val (modules, libJarNames) = GUICode.dependencies
      for ((module, lastModified) <- modules) {writer.println(moduleJarsPrefix + module + ".jar " + lastModified)}
      for ((libName, lastModified) <- libJarNames) {writer.println(externalJarsPrefix + libName + " " + lastModified)}
    } else if (path.startsWith("/" + moduleJarsPrefix)) {
      println("Asking for module Jar: " + path)
      val module = {
        val p = path.replaceFirst("/" + moduleJarsPrefix, "")
        p.substring(0, p.size-4)
      }
      val lastModified = GUICode.lastModifiedForModule(module)

      val moduleJarName = module + "_" + lastModified + ".jar"

      def getOrGenerateFile = {
        val moduleJarFile = new File(fileCacheDir, moduleJarName)
        if (moduleJarFile.exists) {
          moduleJarFile
        } else {
          val outputPath = new File(module + classesPath)
          generateJar(moduleJarFile, outputPath)
          moduleJarFile.setLastModified(lastModified)
          moduleJarFile
        }
      }

      val moduleJarFile = fileCache.memoize(moduleJarName, getOrGenerateFile)
      writeFileToStream(moduleJarFile, response)
    } else if (path.startsWith("/" + externalJarsPrefix)) {
      println("Asking for library Jar: " + path)
      val jarName = path.substring(1+externalJarsPrefix.length)
      val jarFile = GUICode.libJarNames(jarName)
      writeFileToStream(jarFile, response)
    } else if (path.equals("/icon.png")) {
      response.setContentType("image/png")
      IO.copy(classOf[WebStartServlet].getClassLoader.getResourceAsStream("icons/webstart.png"), response.getOutputStream)
    } else if (path == "/" + booterName) {
      println("Asking for booter.jar")
      val (booterJarFile, _) = generateBooterJar
      writeFileToStream(booterJarFile, response)
    } else if (path == "/" + starlingExeName) {
      val iconFile = new File("project/deployment/starling.ico")
      val splashFile = new File("project/deployment/splash_screen.bmp")
      val (booterJarFile,booterTimestamp) = generateBooterJar
      val timestamp = math.max(booterTimestamp, new File("services" + classesPath + "starling/http/InstallationHelper.class").lastModified())
      val exeName = "Starling-" + serverName + "_" + timestamp + ".exe"

      def getOrGenerateFile = {
        val exeFile = new File(fileCacheDir, exeName)
        if (exeFile.exists) {
          println("Returning the exe")
          exeFile
        } else {
          println("Generating the exe")
          val configFile = new File(fileCacheDir, "launch4j.xml")
          val configXML = InstallationHelper.generateL4JXML(externalURL, serverName, booterJarFile.getAbsolutePath, iconFile.getAbsolutePath, splashFile.getAbsolutePath, timestamp).toString
          val bufferedWriter = new BufferedWriter(new FileWriter(configFile))
          bufferedWriter.write(configXML)
          bufferedWriter.close
          println("Config: " + configFile.getAbsolutePath)
          execute("lib/launch4j/launch4j", configFile.getAbsolutePath)
          exeFile
        }
      }
      val exeFile = fileCache.memoize(exeName, getOrGenerateFile)
      writeFileToStream(exeFile, response)
    } else if (path == "/" + kerberosName) {
      val kerberosFile = new File("project/deployment/kerberos.reg")
      writeFileToStream(kerberosFile, response)
    } else if (path == "/" + excelPluginIniName) {
      def generateFile = {
        val iniFile = new File(fileCacheDir, excelPluginIniName + "-" + new Timestamp().toString)
        val bufferedWriter = new BufferedWriter(new FileWriter(iniFile))
        bufferedWriter.write("server=" + xlloopUrl + "\r\n") // I'm using windows carriage return + line feed on purpose here!
        bufferedWriter.write("send.user.info=true\r\n")
        bufferedWriter.close
        iniFile
      }
      val iniFile = fileCache.memoize(excelPluginIniName, generateFile)
      writeFileToStream(iniFile, response)
    } else if (path == "/" + excelPluginXllName) {
      val xlloopFile = new File("project/deployment/xlloop-0.3.1.xll")
      writeFileToStream(xlloopFile, response)
    } else if (path == "/" + starlingInstallerName) {
      val installationFileName = starlingInstallerName
      def generateFile = {
        val installationFile = new File(fileCacheDir, installationFileName + "-" + new Timestamp().toString)
        val text = InstallationHelper.generateNSISText(serverName)
        val bufferedWriter = new BufferedWriter(new FileWriter(installationFile))
        bufferedWriter.write(text)
        bufferedWriter.close()
        installationFile
      }
      val installationFile = fileCache.memoize(installationFileName, generateFile)
      writeFileToStream(installationFile, response)
    } else if (path == "/" + createInstallerName) {
      val readMeFile = new File("project/deployment/InstallationCreator_READ_ME.txt")
      writeFileToStream(readMeFile, response)
    } else {
      response.sendError(404)
    }

    def generateBooterJar = {
      val classes = new File("booter/out")
      val timestamp = GUICode.findLastModified(classes)
      val booterJarName = "booter_" + timestamp + ".jar"

      def getOrGenerateBooterFile = {
        val booterJarFile = new File(fileCacheDir, booterJarName)
        if (booterJarFile.exists) {
          booterJarFile
        } else {
          generateJar(booterJarFile, classes, Some("starling.booter.Booter"))
          signJar(booterJarFile)
          booterJarFile.setLastModified(timestamp)
          booterJarFile
        }
      }
      (fileCache.memoize(booterJarName, getOrGenerateBooterFile), timestamp)
    }

    def writeFileToStream(jarFile:File, response:HttpServletResponse) {
      response.setDateHeader("Last-Modified", jarFile.lastModified)
      response.setContentType("application/octet-stream")
      val inputStream = new BufferedInputStream(new FileInputStream(jarFile))
      IO.copy(inputStream, response.getOutputStream)
      inputStream.close
      response.getOutputStream.flush()
      response.getOutputStream.close()
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


  private def generateJar(jarFile:File, outputPath:File, main:Option[String] = None) {
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
