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

object BuildManager{

  case object WorkAvailable
  case object GiveMeWork
  case object CurrentlyBusy
  case object Execute
  case class RegisterWorker(executor : ActorRef)
  case class UnitOfWork(task : Task, upstreamResults : Iterable[TaskResult], sw : Stopwatch) 
  case class UnitOfWorkResult(task : Task, result : TaskResult)
  case class TimedResults(results : List[TaskResult], clockTime : Long)

  class Worker(manager : ActorRef) extends Actor{

    val log = MakerLog()
    override def preStart{
      manager ! RegisterWorker(self)
    }

    def processRequest(msg : Any){
      msg match {
        case WorkAvailable => {
          manager ! GiveMeWork
        }
        case UnitOfWork(task, upstreamResults, sw) => {
          val result = try {
            task.exec(upstreamResults, sw)
          } catch {
            case e : Exception => 
              log.warn("Error running " + task, e)
              TaskResult(task, sw, succeeded = false, exception = Some(e))
          }
          manager ! UnitOfWorkResult(task, result)
          manager ! GiveMeWork
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

  def props(graph : Dependency.Graph, log : MakerLog = MakerLog()) = Props(classOf[BuildManager], graph, log)

  def build(manager : ActorRef, log : MakerLog = MakerLog()) : Promise[Any] = {
    import akka.pattern.ask
    implicit val timeout = Timeout(5 seconds)
    val future = manager ? Execute
    val promise = Await.result(future, Duration.Inf).asInstanceOf[Promise[Any]]
    promise

  }
}

case class BuildManager(graph : Dependency.Graph, log : MakerLog = MakerLog()) extends Actor{

  import BuildManager._

  var buildLauncher : Option[ActorRef] = None
  var workers : Set[ActorRef] = Set.empty
  var remaining : Dependency.Graph = graph
  var completed : Set[Task] = Set.empty
  var running : Set[Task] = Set.empty
  var results : List[TaskResult] = Nil
  var stopwatch : Stopwatch = Stopwatch()
  var nextResult : Option[Promise[Any]] = None

  private def announceWork = workers.foreach{ _ ! WorkAvailable}
  private def nextTask : Option[Task] = (remaining.leaves -- running).headOption
  def returnResults = {
    val tr = TimedResults(results.reverse, 0)
    nextResult.get.success(tr)
    buildLauncher.get ! tr
  }

  def processRequest(msg : Any){
    msg match {
      case Execute => {
        buildLauncher = Some(sender)
        nextResult = Some(Promise[Any]())
        sender ! nextResult.get

        if (graph.isEmpty)
          returnResults
        else if (workers.isEmpty)
          log.error("Have no workers")
        else {
          stopwatch = Stopwatch()
          announceWork
        }
      }

      case RegisterWorker(worker) => {
        if (workers.contains(worker))
          log.error("Already have this worker")
        else
          workers += worker
      }

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
