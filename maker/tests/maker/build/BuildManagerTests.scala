package maker.build

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.TestActorRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.Timeout
import maker.build.BuildManager._
import maker.build.Dependency._
import maker.task.Task
import maker.task.TaskResult
import maker.utils.MakerLog
import maker.utils.Stopwatch
import org.mockito.Matchers._
import org.mockito.ArgumentMatcher
import org.mockito.Mockito
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.FunSuiteLike
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.slf4j.Logger
import akka.util.duration._
import akka.testkit.ImplicitSender
import java.util.concurrent.atomic.AtomicBoolean
import maker.task.TaskContext
import akka.actor.ExtendedActorSystem
import maker.akka.RemoteActor


class BuildManagerTests extends TestKit(ActorSystem.create("TestActorSystem", RemoteActor.systemConfig)) 
  with FunSpecLike 
  with MockitoSugar 
  with BeforeAndAfterAll
  with ShouldMatchers
  with ImplicitSender
{

  override def afterAll() { system.shutdown() }
  implicit val timeout = Timeout(10 seconds)

  def makeLogger = mock[Logger]

  def randomName = "manager-" + Math.random

  def newManager(graph : Dependency.Graph, workers : Iterable[ActorRef], name : String) : ActorRef = {
    val logger = makeLogger
    val actor = TestActorRef[BuildManager](BuildManager.props("Dummy build name", graph, workers), name)
    actor
  }

  def newWorkerWithLogger = {
    val logger = makeLogger
    val worker = TestActorRef[Worker](Props(new Worker { override val log = MakerLog(logger)}))
    (worker, logger)
  }
  def newWorker = newWorkerWithLogger._1
     
  trait IntTask extends Task{
    def n : Int
    val haveLaunched = new AtomicBoolean(false)
    val name = "Test " + n
    def exec(context : TaskContext) = {
      require(! haveLaunched.get, "Should only launch task once")
      haveLaunched.set(true)
      if (n < 0)
        TaskResult.failure(this, info = Some(n))
      else
        TaskResult.success(this, info = Some(n))
    }
    def upstreamTasks = Nil
  }

  case class DummyTask(n : Int) extends IntTask

  object ExceptionThrowingTask extends Task{
    val name = "ExceptionThrowingTask"
    def exec(context : TaskContext) = {
      throw new RuntimeException("BANG!")
    }
    def upstreamTasks = Nil
  }

  def graphOfUnrelatedTasks(tasks : Task*) : Dependency.Graph = {
    Dependency.Graph(tasks.toSet, Set.empty)
  }

  def newGraph(tasks : List[IntTask]) : Dependency.Graph = {
    val edges = for(
      t1 <- tasks; 
      t2 <- tasks if t2.n != t1.n && t2.n % t1.n == 0
    ) yield Edge(t1, t2)
    Dependency.Graph(tasks.toSet, edges.toSet)
  }

  def newGraph(ns : Int*) : Dependency.Graph = {
    newGraph(ns.toList.map(DummyTask))
  }

  describe("BuildManager"){

    it("Tells workers there is work available"){
      val graph = newGraph(1)
      val workers = List(TestProbe(), TestProbe())
      val manager = newManager(graph, workers.map(_.ref), "WorkTellingManager")
      manager ! Execute
      workers.foreach{_.expectMsg(WorkAvailable)}
    }

  }

  describe("Worker"){

    it("Should request work when there is some available"){
      ignoreMsg{case _ => true}
      val worker = newWorker
      ignoreNoMsg
      worker ! WorkAvailable
      fishForMessage(){
        case GiveMeWork => true
        case _ => false
      }
    }

    it("Should ask for more work when it finishes any"){
      val worker = newWorker
      worker ! UnitOfWork(DummyTask(1), TaskContext(system.asInstanceOf[ExtendedActorSystem], Nil))
      fishForMessage(){
        case GiveMeWork => true
        case _ => false
      }
    }
  }

  private def build(graph : Dependency.Graph, numWorkers : Int) = Build("Dummy name", graph, numWorkers)

  describe("Build"){
    it("should return success on an empty graph"){
      val tr = build(newGraph(), numWorkers = 0).execute
      assert(tr.results.isEmpty)
    }

    it("Should process a single task"){
      val TimedResults(_, _, List(taskResult), _) = build(newGraph(101), numWorkers = 1).execute
      assert(taskResult.info == Some(101))
    }

    it("Should allow one worker to process two tasks with no dependency"){
      val tr@TimedResults(_, _, results, _) = build(newGraph(2, 5), numWorkers = 1).execute
      assert(results.size === 2)
      assert(tr.succeeded)
    }

    it("Should allow one worker to process dependent tasks"){
      val graph = newGraph(2, 4, 6, 8) 
      val tr@TimedResults(_, _, results, _) = build(graph, numWorkers = 1).execute
      assert(results.size === 4)
      assert(tr.succeeded)
    }

    it("Should allow more than one worker to process dependent tasks"){
      val graph = newGraph(2, 4, 6, 8, 10, 12, 14, 16) 
      val tr@TimedResults(_, _, results, _) = build(graph, numWorkers = 3).execute
      assert(results.size === 8)
      assert(tr.succeeded)
    }

    it("Should stop at the first failure - by default"){
      
      val graph = newGraph(2, 4, -6, 12) 
      val tr@TimedResults(_, _, results, _) = build(graph, numWorkers = 1).execute
      assert(tr.failed)
      assert(results.size < 4)
    }

    it("Shouldn't stop the build for a failing task that is configured not to"){
      val failingTask = new DummyTask(-10){
        override def failureHaltsTaskManager = false
      }
      val graph = newGraph(List(DummyTask(2), failingTask, DummyTask(20)))
      val build = Build(name = "dont-stop", graph = graph, numberOfWorkers = 1)
      val TimedResults(_, _, results, _) = build.execute
      assert(results.size === 3)
    }


    it("Shouldn't die if a task throws an exception"){
      val graph = graphOfUnrelatedTasks(ExceptionThrowingTask)
      val (worker, logger) = newWorkerWithLogger
      val manager = newManager(graph, List(worker), "Exception-throwing")
      val TimedResults(_, _, results, _) = Build.execute(manager)
      assert(results.size === 1)
      class IsThrowable extends ArgumentMatcher[Throwable]{
        def matches(obj : Object) = obj match {
          case _ : Throwable => true
          case _ => false
        }
      }
      val anyThrowable = new IsThrowable
      Mockito.verify(logger).warn(anyString(), argThat(anyThrowable))
    }

  }
}
