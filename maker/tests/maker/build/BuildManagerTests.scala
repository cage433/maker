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
import org.mockito.Mockito
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSpecLike
import org.scalatest.FunSuiteLike
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.slf4j.Logger
import scala.concurrent.duration._
import akka.testkit.ImplicitSender
import java.util.concurrent.atomic.AtomicBoolean

class BuildManagerTests extends TestKit(ActorSystem("TestActorSystem")) 
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

  def newManagerWithLogger(graph : Dependency.Graph = Dependency.Graph.empty, name : String = randomName) = {
    val logger = makeLogger
    val actor = TestActorRef[BuildManager](BuildManager.props(graph, MakerLog(logger)), name)
    (logger, actor)
  }
  def newManager(graph : Dependency.Graph = Dependency.Graph.empty, name : String = randomName) = newManagerWithLogger(graph, name)._2

  def newWorker(manager : ActorRef) = TestActorRef[Worker](Props(new Worker(manager) { override val log = MakerLog(makeLogger)}))
     
  trait IntTask extends Task{
    def n : Int
    val haveLaunched = new AtomicBoolean(false)
    val name = "Test " + n
    def exec(upstreamResults : Iterable[TaskResult], sw : Stopwatch) = {
      require(! haveLaunched.get, "Should only launch task once")
      haveLaunched.set(true)
      if (n < 0)
        TaskResult.failure(this, sw, info = Some(n))
      else
        TaskResult.success(this, sw, info = Some(n))
    }
    def upstreamTasks = Nil
  }

  case class DummyTask(n : Int) extends IntTask

  object ExceptionThrowingTask extends Task{
    val name = "ExceptionThrowingTask"
    def exec(upstreamResults : Iterable[TaskResult], sw : Stopwatch) = {
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
    it("Should reject work if it has no attached workers"){
      val graph = newGraph(1)
      val (logger, manager) = newManagerWithLogger(graph)
      manager ! Execute
      Mockito.verify(logger).error(any())
    }

    it("Registers workers"){
      val manager = newManager(newGraph())
      val worker = newWorker(manager)
      manager.underlyingActor.workers should contain(worker)
    }

    it("Tells workers there is work available"){
      val graph = newGraph(1)
      val manager = newManager(graph)
      val workers = Seq(TestProbe(), TestProbe())
      workers.foreach{ worker => manager ! RegisterWorker(worker.ref)}
      manager ! Execute
      workers.foreach{_.expectMsg(WorkAvailable)}
    }

  }

  describe("Worker"){
    it("Registers with a build manager when created"){
      val manager = newManager(newGraph())
      val worker = newWorker(manager)
      manager.underlyingActor.workers should contain(worker)
    }

    it("Should request work when there is some available"){
      ignoreMsg{case _ => true}
      val worker = newWorker(testActor)
      ignoreNoMsg
      worker ! WorkAvailable
      fishForMessage(){
        case GiveMeWork => true
        case _ => false
      }
    }

    it("Should ask for more work when it finishes any"){
      val worker = newWorker(testActor)
      worker ! UnitOfWork(DummyTask(1), Nil, Stopwatch.global)
      fishForMessage(){
        case GiveMeWork => true
        case _ => false
      }
    }
  }

  describe("Build"){
    it("should return success on an empty graph"){
      val manager = newManager(newGraph())
      val worker = newWorker(manager)
      manager ! Execute
      fishForMessage(){
        case TimedResults(results, _)  => results.isEmpty
        case _ => false
      }
    }

    it("Should process a single task"){
      val manager = newManager(newGraph(101))
      val worker = newWorker(manager)
      manager ! Execute
      fishForMessage(){
        case TimedResults(List(taskResult), _)  => taskResult.info === Some(101)
        case other => false
      }
    }

    it("Should allow one worker to process two tasks with no dependency"){
      val manager = newManager(newGraph(2, 5))
      val worker = newWorker(manager)
      manager ! Execute
      val TimedResults(results, _) = fishForMessage(){
        case tr : TimedResults => true
        case other => false
      }
      assert(results.size === 2)
    }

    it("Should allow one worker to process dependent tasks"){
      val graph = newGraph(2, 4, 6, 8) 
      val manager = newManager(graph)
      val worker = newWorker(manager)
      manager ! Execute
      val TimedResults(results, _) = fishForMessage(){
        case tr : TimedResults => true
        case other => false
      }
      assert(results.size === 4)
    }

    it("Should allow more than one worker to process dependent tasks"){
      val graph = newGraph(2, 4, 6, 8, 10, 12, 14, 16) 
      val manager = newManager(graph)
      newWorker(manager)
      newWorker(manager)
      newWorker(manager)
      manager ! Execute
      val TimedResults(results, _) = fishForMessage(){
        case tr : TimedResults => true
        case other => false
      }
      assert(results.size === 8)
    }

    it("Should stop at the first failure - by default"){
      
      val graph = newGraph(2, 4, -6, 12) 
      val manager = newManager(graph)
      newWorker(manager)
      manager ! Execute
      val TimedResults(results, _) = fishForMessage(){
        case tr : TimedResults => true
        case other => false
      }
      assert(results.size < 4)
    }

    it("Shouldn't stop the build for a failing task that is configured not to"){
      val failingTask = new DummyTask(-10){
        override def failureHaltsTaskManager = false
      }
      val graph = newGraph(List(DummyTask(2), failingTask, DummyTask(20)))
      val manager = newManager(graph, "dont-stop")
      newWorker(manager)
      val TimedResults(results, _) = BuildManager.execute(manager)
      assert(results.size === 3)
    }

    it("Shouldn't die if a task throws an exception"){
      val graph = graphOfUnrelatedTasks(ExceptionThrowingTask)
      val manager = newManager(graph, "Exception-throwing")
      newWorker(manager)
      val TimedResults(results, _) = BuildManager.execute(manager)
      assert(results.size === 1)
    }

  }
}
