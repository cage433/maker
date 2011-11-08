package starling.webservice

import starling.utils.ImplicitConversions._
import xml.{Text, Elem, XML}
import dispatch._
import starling.props.Props

object QlikViewUpdater {
  def main(args: Array[String]) {
    new QlikViewUpdater("http://qvdev:8080/qmsb/Task.asmx", "Reload of Corporate.IT.QV.Deploy.Extract.qvw (UAT)").update
  }
}

class QlikViewUpdater(serverUrl: String, taskName: String) {
  def this(props: Props) = this(props.QlikViewServerUrl(), props.QlikViewSpotFXTask())

  require(taskName.trim.nonEmpty, "Missing task")

  def update: Unit = runTask(taskID(taskName).getOrThrow("No such task: " + taskName))

  private def taskID(taskName: String): Option[String] = XML.load(urlOf("GetTaskID") + "?taskName=" + taskName) partialMatch {
    case Elem(_, "string", _, _, Text(guid)) => guid
  }

  private val h = new Http
  private def runTask(taskID: String) = h(url(urlOf("RunTask")).POST << Map("taskID" → taskID) as_str)
  private def urlOf(operation: String) = serverUrl + "/" + operation
}