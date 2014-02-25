package maker.build

import maker.utils.MakerLog
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import maker.task.Task
import akka.actor.ActorRef
import maker.utils.Stopwatch
import maker.task.TaskResult
import akka.dispatch.Promise
import akka.actor.ActorSystem
import akka.dispatch.Await
import akka.util.Duration
import maker.task.test.TestResults
import akka.dispatch.Future
import akka.pattern.ask
import maker.project.Module
import maker.project.BaseProject
import maker.task.TaskContext
import akka.actor.ExtendedActorSystem
import maker.scalatest.TestReporterActor

object BuildManager{

  case object WorkAvailable
  case object GiveMeWork
  case object Execute
  case class UnitOfWork(task : Task, context : TaskContext) 
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
    def props() = Props(new Worker())
  }

  class Worker extends Actor{

    val log = MakerLog()

    def processRequest(msg : Any){
      msg match {
        case WorkAvailable => {
          sender ! GiveMeWork
        }
        case UnitOfWork(task, taskContext) => {
          val result = try {
            task.exec(taskContext)
          } catch {
            case e : Exception => 
              log.warn("Error running " + task, e)
              TaskResult(task, succeeded = false, exception = Some(e))
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

  def props(buildName : String, graph : Dependency.Graph, workers : Iterable[ActorRef]) = Props(new BuildManager(buildName, graph, workers))

}


case class BuildManager(buildName : String, graph : Dependency.Graph, workers : Iterable[ActorRef]) extends Actor{

  import BuildManager._
  import TestReporterActor._

  if (graph.nonEmpty)
    require(workers.nonEmpty, "Can't execute non empty graph of tasks without workers")

  private def announceWork = workers.foreach{wkr =>  wkr ! WorkAvailable}

  implicit val executionContext = context.dispatcher
  private val resultPromise = Promise[TimedResults]()
  private var remaining : Dependency.Graph = graph
  private var completed : Set[Task] = Set.empty
  private var stopwatch : Stopwatch = Stopwatch()
  private var results : List[TaskResult] = Nil
  private var running : Set[Task] = Set.empty

  type ModuleName = String
  private var runningTests : List[(ModuleName, ActorRef)] = Nil

  def returnResults() = {
    val tr = TimedResults(buildName, graph, results.reverse, 0)
    resultPromise.success(tr)
  }

  private def failed() : Receive = {

    if (running.isEmpty)
      returnResults()

    {
      case UnitOfWorkResult(task, result) =>{
        results = result :: results
        running -= task
        if (running.isEmpty){
          returnResults()
        }
      }

      case _ => 
    }
  }
  private def executing():Receive = {

    def nextTask : Option[Task] = (remaining.leaves -- running).headOption


    announceWork

    {
      case UnitOfWorkResult(task, result) =>{
        completed += task
        running -= task
        remaining -= task
        results = result :: results
        if (result.failed && task.failureHaltsTaskManager){
          context.become(failed())
        } else {
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
            sender ! UnitOfWork(task, TaskContext(context.system.asInstanceOf[ExtendedActorSystem], results))
          case None => 
        }
      }

      case ModuleTestsStarted(moduleName) => 
        runningTests = (moduleName, sender) :: runningTests

      case ModuleTestsFinished(moduleName) => 
        runningTests = runningTests.filterNot(_._1 == moduleName)

      case BuildInteractor.KeyPressed(100) =>
        runningTests.lastOption.foreach{
          case (_, actorRef) => 
            actorRef ! DumpTestThread
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
