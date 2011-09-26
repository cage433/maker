package starling.launcher

import starling.http.GUICode
import starling.props.{PropsHelper, Props}
import java.net.{BindException, ServerSocket}
import starling.api.utils.PropertiesMapBuilder
import starling.startserver.Server
import starling.launcher.Launcher

object DevLauncher {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props = propsWithUnusedPort()
    Server.run()
    //System.setProperty(BouncyRMI.CodeVersionKey, GUICode.latestTimestamp.toString())
    Launcher.start(props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName())
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

  private def propsWithUnusedPort() = {
    val fileProps = PropertiesMapBuilder.allProps
    val starlingProps = PropertiesMapBuilder.starlingProps
    val trafiguraProps = PropertiesMapBuilder.trafiguraProps

    var counter = 1
    var props = new Props(starlingProps, trafiguraProps)

    val localPorts = props.propertiesOfType(classOf[PropsHelper#LocalPort]).keySet

    val serverName = props.ServerName()
    while (!rmiPortAvailable(props)) {
      counter += 1
      props = new Props(fileProps.filterKeys(p=>{!localPorts.contains(p.toLowerCase)}) + ("ServerName"->(serverName + " " + counter)), trafiguraProps)
    }
    props
  }
}

object LauncherAlone{
  def main(args: Array[String]) {
    val props = PropsHelper.defaultProps
    Launcher.start(props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName())
  }
}

object DevRMILauncher {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val props = PropsHelper.defaultProps
    System.setProperty("starling.codeversion.timestamp", GUICode.latestTimestamp.toString())
    Launcher.start(props.ExternalHostname(), props.RmiPort(), props.ServerPrincipalName())
  }
}
