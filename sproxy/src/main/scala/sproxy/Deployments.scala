package sproxy

import java.io._
import java.util.{HashMap, LinkedList, Properties}

/**
 * These classes encapsulate the deployments directory.
 * 
 * The servlet provides a view onto this.
 * 
 * The view provided is a list of the directories and their status.
 * For the matching and valid deployements it provides the port to proxy to. 
 */
case class Deployment
case class ValidDeployment(name:String,dir:File,port:Int,deploymentDetails:String) extends Deployment
case class ErrorDeployment(name:String,dir:File,message:String,deploymentDetails:String) extends Deployment

class Deployments(val name:String, directories:List[(String,File)]) {
  var mapped = new HashMap[String,ValidDeployment]
  var dirs = new LinkedList[Deployment]
  
  def reload() = {
    mapped.clear
    dirs.clear
    directories.foreach( (t) => {
      t._2.listFiles.foreach( (file) => {
        val deployment = createDeployment(t._1, file)
        deployment match {
          case d:ValidDeployment => { mapped.put(d.name, d) }
          case _ =>
        }
        dirs.add(deployment)
      })
    })
  }

  private def createDeployment(prefix:String, root:File) = {
    //git based deployments have the core directory
    //old style /opt/starling deployments use classes in the root directory
    val file = if (!new File(root, "generated.props.conf").exists() &&
      new File(root, "core").exists) {
        new File(root, "core") } else { root}
    val name = prefix + root.getName
    val propertiesFile = new File(file, "generated.props.conf")
    if (propertiesFile.exists()) {
      val properties = new Properties()
      properties.load(new FileInputStream(propertiesFile));
      if (new File(file, "props.conf").exists()) {
        properties.load(new FileInputStream(new File(file, "props.conf")))
      }
      val deploymentDetails = readDeploymentDetails(root)
      val port = properties.getProperty("HttpPort")
      if (port == null) {
        ErrorDeployment(name, root, "no HttpPort property", deploymentDetails)
      } else {
        val p = Integer.parseInt(port)
        ValidDeployment(name, root, p, deploymentDetails)
      }
    } else {
      ErrorDeployment(name, root, "No generated.props.conf", "")
    }
  }
  def readDeploymentDetails(dir:File) = {
    val deploymentDetailsFile = new File(dir, "deployment_details")
    if (!deploymentDetailsFile.exists) {
      "deployment_details not found"
    } else {
      val builder = new StringBuilder
      val raf = new RandomAccessFile(deploymentDetailsFile, "r")
      var line = raf.readLine
      while (line != null) {
        builder.append(line)
        builder.append("\n")
        line = raf.readLine
      }
      builder.toString
    }
  }
  def lookup(name:String):Option[Int] = {
    var port = mapped.get(name)
    if (port==null) { None } else { Some(port.port) }
  }
  def valid_deployments() = {
    mapped.keySet.toArray(new Array[String](0))
  }
  def noValidDeployments = {
    mapped.isEmpty
  }
  def nothing = {
    dirs.isEmpty
  }
}
