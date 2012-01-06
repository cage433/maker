package starling.launcher

import starling.http.GUICode
import starling.props.{PropsHelper, Props}
import java.net.{BindException, ServerSocket}
import starling.startserver.Server
import starling.api.utils.{PropertyValue, PropertiesMapBuilder}
import starling.bouncyrmi.BouncyRMI

object DevLauncher {
  def main(args:Array[String]) {
    val buffer = Launcher.teeStdOut
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props = propsWithUnusedPort()
    System.setProperty("appname", props.ServerName())
    Server.run
    System.setProperty(BouncyRMI.CodeVersionKey, GUICode.allMD5s)
    Launcher.start(buffer, props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName(), props.ServerType())
  }

  private def rmiPortAvailable(props:Props) = {
    try {
      try {
        new ServerSocket(props.RmiPort()).close()
        true
      } catch {
        case e : BindException => false
      }
    }
  }

  def propsWithUnusedPort() = {
    val fileProps = PropertiesMapBuilder.allProps
    val starlingProps = PropertiesMapBuilder.starlingProps
    val trafiguraProps = PropertiesMapBuilder.trafiguraProps

    var counter = 1
    var props = new Props(starlingProps, trafiguraProps)

    val localPorts = props.propertiesOfType(classOf[PropsHelper#LocalPort]).keySet

    val serverName = props.ServerName()
    while (!rmiPortAvailable(props)) {
      counter += 1
      val propValue = PropertyValue("generated from ServerName", serverName + " " + counter)
      props = new Props(fileProps.filterKeys(p=>{!localPorts.contains(p.toLowerCase)}) + ("ServerName"->(propValue)), trafiguraProps)
    }
    props
  }
}

object LauncherAlone{
  def main(args: Array[String]) {
    val buffer = Launcher.teeStdOut
    val props = PropsHelper.defaultProps
    Launcher.start(buffer, props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName(), props.ServerType())
  }
}

object DevRMILauncher {
  def main(args:Array[String]) {
    val buffer = Launcher.teeStdOut
    val props = PropsHelper.defaultProps
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    System.setProperty(BouncyRMI.CodeVersionKey, GUICode.allMD5s)
    System.setProperty("appname", props.ServerName())
    Launcher.start(buffer, props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName(), props.ServerType())
  }
}
