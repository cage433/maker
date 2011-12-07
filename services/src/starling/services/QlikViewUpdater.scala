package starling.services

import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._
import dispatch._
import starling.props.Props
import xml._
import scalaz.Scalaz._
import swing.event.Event
import starling.gui.api.SpotFXDataEvent
import starling.manager.Receiver
import starling.utils.{RetryingAction, Log}
import starling.daterange.Notifier

object QlikViewUpdater {
  def main(args: Array[String]) {
    new QlikViewUpdater("http://qvdev:8080/qmsb/Task.asmx", "Reload of Titan.PricingAndRisk.FXRates.Extract.qvw")
      .event(SpotFXDataEvent(null, null, null, false))
  }

  def extractTaskID(taskName: String, tasks: Elem): String = {
    val nameToID = (tasks \\ "TaskInfo").map(task => ((task \ "Name").text, (task \ "ID").text)).toMap

    nameToID.filterKeys(_.contains(taskName)).toList match {
      case List((name, id)) => id
      case Nil => throw new Exception("No task with name containing: '%s', available:\n\t%s" % (taskName, nameToID.keys.mkString("\n\t")))
      case many => throw new Exception("%d tasks with name containing: '%s'\n\t%s" % (many.size, taskName, many.map(_._1).mkString("\n\t")))
    }
  }
}

class QlikViewUpdater(serverUrl: String, spotFXTaskName: String, notifier: Notifier = Notifier.Null) extends Receiver with Log {
  def this(props: Props) = this(props.QlikViewServerUrl(), props.QlikViewSpotFXTask())

  def event(event: Event) = event partialMatch {
    case _: SpotFXDataEvent => try {
      notifier.expectWithin("latestSpotFXRates", 2000, () => log.error("web service failed to call latestSpotFXRates"))

      log.info("Notifying web service: %s, task: %s" % (serverUrl, spotFXTaskName))
      runTask(getTaskID())
    } catch {
      case e => log.error("Unable to update", e)
    }
  }

  private val h = new Http
  private val getTaskID = new RetryingAction(() => QlikViewUpdater.extractTaskID(spotFXTaskName, XML.load(urlOf("GetTasks"))))
  private def runTask(taskID: String) = h(url(urlOf("RunTask")).POST << Map("taskID" → taskID) as_str)
  private def urlOf(operation: String) = serverUrl + "/" + operation

  safely(getTaskID()) // Warn if early binding isn't possible
    .fold(ex => {ex.printStackTrace(); log.warn(ex.getMessage)}, id => log.debug("QlikView task containing: \"%s\" has id: %s" % (spotFXTaskName, id)))
}