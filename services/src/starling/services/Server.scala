package starling.services

import starling.props.{PropsHelper, Props}
import java.io.File
import starling.utils._
import java.lang.String

/**
 * The main entry point into Starling
 */
object Server extends OutputPIDToFile {
  def main(args: Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    ServerHelper.deleteRunningFile
    PropsHelper.writeDefaults
    run(PropsHelper.defaultProps, args)
    ServerHelper.createRunningFile
  }

  var server: StarlingInit = null

  def run(props: Props, args: Array[String] = Array[String]()) {
    Log.infoWithTime("Launching starling server") {

      server = new StarlingInit(props, true, true, true, startEAIAutoImportThread = props.ImportsBookClosesFromEAI(),
        startRabbit = props.RabbitEnabled())
      server.start
    }
  }
}

object ServerHelper {
  val RunningFileName = "starling.running"


  def createRunningFile {
    val file = new File(RunningFileName)
    if (!file.createNewFile) throw new Exception("Couldn't create the " + RunningFileName + " file")
  }

  def deleteRunningFile {
    val file = new File(RunningFileName)
    if (file.exists) file.delete
  }
}
