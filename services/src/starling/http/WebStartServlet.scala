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

  val scalaLibraryJar = new File("lib/scala/scala-2.9.0.1.final/lib/scala-library.jar")

  // The order of this list matters. It is the order things are attempted to be loaded so ensure it is optimised.
  val modules = List("daterange", "quantity", "utils", "auth", "bouncyrmi", "gui", "gui.api", "pivot")

  // We know that in practise these jars are not used by the GUI
  val bigJarsThatAreNotRequired = Set(
    "colt-1.2.0.jar", "testng-5.8-jdk15.jar", "commons-collections-3.2.1.jar",
    "scalatest-1.0.jar", "cron4j-2.2.1.jar", "amqp-client-1.7.0.2.jar")

  val libJarNames = Map(
    "scala-library.jar" -> scalaLibraryJar,
    "scala-swing.jar" -> new File("lib/scala/scala-2.9.0.1.final/lib/scala-swing.jar")
  ) ++ modules.flatMap(module => {
    val lib = new File(module + "/lib/")
    val libManaged = new File(module + "/lib_managed/scala_2.9.0-1/compile/")
    def jarsFromDir(dir : File) = {
      if (dir.exists) {
        val jarFiles = dir.listFiles.toList.filter(_.getPath.endsWith(".jar")).filterNot{path => {
          bigJarsThatAreNotRequired.contains(path.getName)
        }}
        jarFiles.map{ file => file.getPath.replaceAll("/", "-") -> file}
      } else {
        List()
      }
    }
    jarsFromDir(lib) ::: jarsFromDir(libManaged)
  })

  def dependencies = {
    (modules.map { module => module -> lastModifiedForModule(module) }, libJarNames.mapValues(file=>file.lastModified))
  }

  def latestTimestamp : Long = {
    val (modules, libraries) = dependencies
    (modules ++ libraries).maximum(_._2)
  }

  def lastModifiedForModule(module:String) = {
    val outputPath = new File(module + "/target/scala_2.9.0-1/classes/")
    val resourcesPath = new File(module + "/resources")
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
      writer.println("starling.gui.Launcher " + PropsHelper.defaultProps.ExternalHostname() + " " + PropsHelper.defaultProps.RmiPort() + " " + PropsHelper.defaultProps.ServerPrincipalName())
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
          val outputPath = new File(module + "/target/scala_2.9.0-1/classes/")
          val resourcesPath = new File(module + "/resources")
          generateJar(moduleJarFile, outputPath, resourcesPath)
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
      println("Generate the exe!")

      val iconFile = new File("project/deployment/starling.ico")
      val splashFile = new File("project/deployment/splash_screen.bmp")
      val (booterJarFile,timestamp) = generateBooterJar
      val exeName = "Starling-" + serverName + "_" + timestamp + ".exe"

      def getOrGenerateFile = {
        val exeFile = new File(fileCacheDir, exeName)
        if (exeFile.exists) {
          exeFile
        } else {
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
          generateJar(booterJarFile, classes, new File("no"), Some("starling.booter.Booter"))
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


  private def generateJar(jarFile:File, outputPath:File, resourcesPath:File, main:Option[String] = None) {
    val jarOutputStream = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(jarFile)))
    for (dir <- List(outputPath, resourcesPath)) {
      if (dir.exists) {
        val files = GUICode.findMatchingFiles(dir).map(_.getPath)
        for (outputFile <- files) {
          jarOutputStream.putNextEntry(new JarEntry(outputFile.substring(dir.getPath.length+1)))
          val inputStream = new BufferedInputStream(new FileInputStream(outputFile))
          IO.copy(inputStream, jarOutputStream)
          inputStream.close
        }
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
