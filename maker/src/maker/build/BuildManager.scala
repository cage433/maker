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

  def execute(manager : ActorRef, log : MakerLog = MakerLog()) : TimedResults = {
    import akka.pattern.ask
    implicit val timeout = Timeout(5 seconds)
    val future = manager ? Execute
    Await.result(future, Duration.Inf).asInstanceOf[TimedResults]
  }
}


case class BuildManager(graph : Dependency.Graph, log : MakerLog = MakerLog()) extends Actor{

  import BuildManager._

  var workers : Set[ActorRef] = Set.empty

  private def announceWork = workers.foreach{ _ ! WorkAvailable}

  private def acceptWorker(worker : ActorRef){
    if (workers.contains(worker))
      log.error("Already have this worker")
    else
      workers += worker
  }

  override def preStart() {
    context.become(awaiting)
  }

  def awaiting : Receive = {
    case Execute => {

      if (graph.isEmpty)
        sender ! TimedResults(Nil, 0)
      else if (workers.isEmpty)
        log.error("Have no workers")
      else {
        context.become(executing(sender))
      }
    }

    case RegisterWorker(worker) => {
      acceptWorker(worker)
    }


    case _ : UnitOfWorkResult => 
      // Should be from an early build that was finished early - can be ignored


  }

  def executing(buildLauncher : ActorRef):Receive = {
    var remaining : Dependency.Graph = graph
    var completed : Set[Task] = Set.empty
    var running : Set[Task] = Set.empty
    var results : List[TaskResult] = Nil
    var stopwatch : Stopwatch = Stopwatch()

    def nextTask : Option[Task] = (remaining.leaves -- running).headOption


    def returnResults = {
      val tr = TimedResults(results.reverse, 0)
      buildLauncher ! tr
      context.become(awaiting)
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

      case RegisterWorker(worker) => 
        acceptWorker(worker)
        announceWork

      case Execute =>
        log.error("Already executing")
    }
  }

  def receive = {
    case msg => 
      log.error("Should already have become something else")
  }

}
