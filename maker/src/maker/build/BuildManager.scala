package maker.build

import maker.utils.MakerLog
import akka.actor.Actor
import maker.akka.MakerActorSystem
import akka.actor.Props
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout
import maker.task.Task
import akka.actor.ActorRef
import maker.utils.Stopwatch
import maker.task.TaskResult
import scala.concurrent.Promise
import akka.actor.ActorSystem
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import maker.task.test.TestResults
import scala.concurrent.Future
import akka.pattern.ask
import maker.project.Module
import maker.project.BaseProject

object BuildManager{

  case class AcknowledgeMeAsYourMaster(master : ActorRef)
  case object WorkAvailable
  case object GiveMeWork
  case object CurrentlyBusy
  case object Execute
  case class UnitOfWork(task : Task, upstreamResults : Iterable[TaskResult], sw : Stopwatch) 
  case class UnitOfWorkResult(task : Task, result : TaskResult)
  case class TimedResults(buildName : String, graph : Dependency.Graph, results : List[TaskResult], clockTime : Long){
    def failed = results.exists(_.failed)
    def succeeded = results.forall(_.succeeded)
    def testResults() : TestResults = {
      var trs : List[TestResults] = Nil
      val allTestResults : List[TestResults] = 
        results.flatMap(_.info).collect{
          case tr : TestResults => tr
      }
      allTestResults.foldLeft(TestResults.EMPTY)(_++_)
    }
    override def toString = buildName + " " + (if (succeeded) "succeeded" else "failed")
  }

  object Worker{
    def props() = Props(classOf[Worker])
  }

  class Worker extends Actor{

    val log = MakerLog()

    def processRequest(msg : Any){
      msg match {
        case WorkAvailable => {
          sender ! GiveMeWork
        }
        case UnitOfWork(task, upstreamResults, sw) => {
          val result = try {
            task.exec(upstreamResults, sw)
          } catch {
            case e : Exception => 
              log.warn("Error running " + task, e)
              TaskResult(task, sw, succeeded = false, exception = Some(e))
          }
          sender ! UnitOfWorkResult(task, result)
          sender ! GiveMeWork
        }
        case _ => 
          log.error("Unexpected message " + msg)
      }
    }

    def receive = {
      case msg => 
        try {
          processRequest(msg)
        } catch {
          case e : Exception => 
            log.error("couldn't process " + msg, e)
            sender ! e
        }
    }
  }

  def props(buildName : String, graph : Dependency.Graph, workers : Iterable[ActorRef], log : MakerLog = MakerLog()) = Props(classOf[BuildManager], buildName, graph, workers, log)

  def execute(manager : ActorRef, moduleAndGraph : Option[(BaseProject, Dependency.Graph)] = None, log : MakerLog = MakerLog()) : TimedResults = {
    moduleAndGraph.foreach{
      case (m, g) => 
        m.setUp(g)
    }
    implicit val timeout = Timeout(2 seconds)
    val future : Future[Promise[TimedResults]] = (manager ? Execute).mapTo[Promise[TimedResults]]
    val resultFuture = Await.result(future, 2 seconds).future
    val tr = Await.result(resultFuture, Duration.Inf).asInstanceOf[TimedResults]
    moduleAndGraph.foreach{
      case (m, g) => 
        m.tearDown(g, tr)
    }
    tr
  }
}


case class BuildManager(buildName : String, graph : Dependency.Graph, workers : Iterable[ActorRef], log : MakerLog = MakerLog()) extends Actor{

  import BuildManager._

  if (graph.nonEmpty)
    require(workers.nonEmpty, "Can't execute non empty graph of tasks without workers")

  private def announceWork = workers.foreach{wkr =>  wkr ! WorkAvailable}

  val resultPromise = Promise[TimedResults]()

  def executing():Receive = {
    var remaining : Dependency.Graph = graph
    var completed : Set[Task] = Set.empty
    var running : Set[Task] = Set.empty
    var results : List[TaskResult] = Nil
    var stopwatch : Stopwatch = Stopwatch()

    def nextTask : Option[Task] = (remaining.leaves -- running).headOption


    def returnResults = {
      val tr = TimedResults(buildName, graph, results.reverse, 0)
      resultPromise.success(tr)
    }

    announceWork

    {
      case UnitOfWorkResult(task, result) =>{
        completed += task
        running -= task
        remaining -= task
        results = result :: results
        if (result.failed && task.failureHaltsTaskManager)
          returnResults
        else {
          nextTask match {
            case Some(_) => announceWork
            case None if running.isEmpty => returnResults
            case _ => 
          }
        }
      }

      case GiveMeWork => {
        nextTask match {
          case Some(task) => 
            running += task
            sender ! UnitOfWork(task, results, stopwatch)
          case None => 
        }
      }
    }
  }

  def receive = {
    case Execute => {

      sender ! resultPromise
      if (graph.isEmpty)
        resultPromise.success(TimedResults(buildName, graph, Nil, 0))
      else {
        context.become(executing())
      }
    }
  }

}
