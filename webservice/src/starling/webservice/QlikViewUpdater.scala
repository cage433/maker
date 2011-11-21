package starling.webservice

import starling.utils.ImplicitConversions._
import dispatch._
import starling.props.Props
import xml._
import scalaz.Scalaz._


object QlikViewUpdater {
  def main(args: Array[String]) {
    new QlikViewUpdater("http://qvdev:8080/qmsb/Task.asmx", "Reload of Titan.PricingAndRisk.FXRates.Extract.qvw (UAT)").update
  }

  def taskID(taskName: String, tasks: Elem) = (tasksWithNameContaining(taskName, tasks) \ "ID").toList match {
    case Nil => failure("No task with name containing: " + taskName)
    case List(singleMatch) => success(singleMatch.text)
    case many => failure("%d tasks with name containing: %s" % (many.size, taskName))
  }

  private def tasksWithNameContaining(taskName: String, tasks: Elem): NodeSeq = {
    (tasks \\ "TaskInfo" filter(_.exists(task => (task \ "Name").text contains(taskName))))
  }
}

class QlikViewUpdater(serverUrl: String, taskName: String) {
  import QlikViewUpdater._
  def this(props: Props) = this(props.QlikViewServerUrl(), props.QlikViewSpotFXTask())

  require(taskName.trim.nonEmpty, "Missing task")

  def update: Unit = runTask(taskID(taskName, XML.load(urlOf("GetTasks"))).fold(error => throw new Exception(error)))

  private val h = new Http
  private def runTask(taskID: String) = h(url(urlOf("RunTask")).POST << Map("taskID" → taskID) as_str)
  private def urlOf(operation: String) = serverUrl + "/" + operation
}