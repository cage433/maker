package sproxy

import java.io._
import java.util.Properties

import management.ManagementFactory
import org.mortbay.jetty.{Server, Handler}
import org.mortbay.jetty.servlet.{ServletHolder, Context}

import javax.servlet.http._

import org.apache.commons.cli._

import scala.Option
import scala.Some
import scala.None

/**
 * This is the entry point to bbproxy.
 * This class just reads command line and bbproxy.conf parameters
 * and starts the server.
 */
object Run {
  def main(args:Array[String]) = {
    
    val options = new Options
    options.addOption("p", "port", true, "the port to listen on (default 5555)")    
    options.addOption("d", "dir", true, "the deployments directory (eg. /opt/starling)")    
    options.addOption("n", "name", true, "the name for this proxy")
    options.addOption("f", "file", true, "read settings from a properties file")
    options.addOption("h", "help", false, "print this help message")
    options.addOption("r", "reload", false, "reloads the deployments on every http request")
    
    val commandLine = new BasicParser().parse(options, args)
    var propertiesFile = new File("bbproxy.conf")
    if (commandLine.hasOption("file")) {
      propertiesFile = new File(commandLine.getOptionValue("file"))
      if (!propertiesFile.exists()) {
        println("Not reading properties file. File not found: " + propertiesFile)
      }
    }
    val properties = new Properties()
    if (propertiesFile.exists()) {
      properties.load(new FileInputStream(propertiesFile));
    }
    commandLine.getOptions.foreach( (option) => {
      if (option.hasArg) {
        val value = commandLine.getOptionValue(option.getLongOpt)
        properties.setProperty(option.getLongOpt, value)                                  
      } else {
println("Setting " + option)
        properties.setProperty(option.getLongOpt, "true")
      }
      println("Op: " + option + "|" + option.hasArg + "|" + option.getLongOpt)
    })
println(properties)
      
    if (commandLine.hasOption("help")) {

        new HelpFormatter().printHelp(
          "bbproxy",
          "",
          options,
          "   if present properties are read from bbproxy.conf\n" +
          "   this should be a properties file with property names matching\n" +
          "   the command line names",
          false)
        
      } else {
        val port = Integer.parseInt(properties.getProperty("port", "5555"))
        val directories = findDirectories(properties.getProperty("dir"))
        val name = properties.getProperty("name", "starling proxy")
        val reload = java.lang.Boolean.valueOf(properties.getProperty("reload", "false")).booleanValue
      
        println("Running bbproxy")
        println
        println("  settings...")
        println("    port : " + port)
        println("    dir  : " + properties.getProperty("dir"))
        println("    name : " + name)
        println("  reload : " + reload)
        println
        run(directories, port, name, reload)
      }
  }

  def findDirectories(extra:String) = {
    val directories = new scala.collection.mutable.ListBuffer[(String,File)]()
    new File("/home").listFiles.foreach( (homeDir) => {
      val starlingDir = new File(homeDir, "starling")
      if (starlingDir.isDirectory) {
        directories += ((homeDir.getName+"-", starlingDir))
      }
    })
    if (extra != null && new File(extra).isDirectory) {
      directories += (("", new File(extra)))
    }
    directories.toList
  }

  def writePID = {
    val processName = ManagementFactory.getRuntimeMXBean.getName
    val pid = processName.subSequence(0, processName.indexOf("@")).toString

    val file = new File("pid.txt")
    if (file.exists) file.delete
    val out = new java.io.FileWriter(file)
    out.write(pid)
    out.close
  }
  
  def run(directories:List[(String,File)],port:int,name:String,reload:Boolean) {
    var deployments = new Deployments(name, directories)
    deployments.reload

    var server = new Server(port)
    var root = new Context(server, "/", 0)
    root.addServlet(new ServletHolder(new MainServlet(deployments, reload)), "/")
    server.addHandler(root)
    server.start
    writePID
  }
}
