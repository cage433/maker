package starling.concurrent

import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import akka.actor.Actor._
import java.util.Random
import java.util.concurrent.LinkedBlockingQueue
import akka.actor.{ActorRef, Actor, ActorRegistry, SupervisorFactory}
import java.util.concurrent.atomic.AtomicInteger
import akka.config.Supervision.{Permanent, OneForOneStrategy, SupervisorConfig, Supervise}
import collection.mutable.ArrayBuffer
import akka.dispatch.{Future, CompletableFuture, MessageDispatcher, Dispatchers}

trait Work[T] {
  val id: Long

  def doWork: Result[T]
}

case class Result[T](id: Long, result: T)

class Worker(name: String, dispatcher: MessageDispatcher) extends Actor {
  self.id = name
  self.dispatcher = dispatcher

  def receive = {
    case work: Work[_] => {
      val result = try {
        work.doWork
      }
      catch {
        case e => e
      }
      self.reply(result)
    }
    case _ =>
  }
}

class ActorSupervisor {

  val ACTORS_COUNT = 200

  val pool = Dispatchers.newExecutorBasedEventDrivenDispatcher("pooled-dispatcher")
    .withNewBoundedThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity(10000)
    .setCorePoolSize(16)
    .setMaxPoolSize(48)
    .setKeepAliveTimeInMillis(60000)
    .setRejectionPolicy(new CallerRunsPolicy)
    .build

  /*
  * Creates list of actors the will be supervised
  */
  lazy val supervisedActors: Array[Supervise] = {
    (1 to ACTORS_COUNT toArray).map{
      i => Supervise(actorOf(new Worker("worker." + i, pool)), Permanent)
    }
  }

  lazy val supervisor = SupervisorFactory(
    SupervisorConfig(
      OneForOneStrategy(List(classOf[Exception]), 5, 1000),
      supervisedActors.toList)).newInstance

  def start {
    // Starts supervisor and all supervised actors
    supervisor.start
  }

  def stop {
    supervisor.shutdown
  }

  /**
   * Central point for sending work. This actor then splits up the work and sends it to WorkActors.
   *
   * This class could be a *lot* smarter, there is no load-balancing, no bunching of tasks, no real error handling, no
   * order preservation...
   */
  private class DispatchingActor[T](work: Traversable[Work[T]]) extends Actor {
    val remaining = new AtomicInteger(work.size)
    val results = ArrayBuffer[Either[Throwable, Result[T]]]()
    var requester: Option[CompletableFuture[Any]] = None

    private val rand = new Random(1234)

    private def worker = {
      // randomly pick an actor to send work to.
      supervisedActors(rand.nextInt(ACTORS_COUNT)).actorRef
    }

    protected def receive = {
      case "start" => {
        requester = Some(self.senderFuture.get)
        for (w <- work) {
          worker ! w
        }
      }
      case reply => {
        reply match {
          case e: Throwable => results += Left(e)
          case r: Result[_] => results += Right(r.asInstanceOf[Result[T]])
        }
        if (remaining.decrementAndGet == 0) {
          (requester: @unchecked) match {
            case Some(r) => r completeWithResult results
          }
        }
      }
    }
  }

  def dispatcher[T](work: Traversable[Work[T]]) = {
    work match {
      case Nil => List[Either[Throwable, Result[T]]]()
      case _ => {
        val actor = actorOf(new DispatchingActor(work))
        actor.start
        val result = actor !! "start"
        actor.stop
        result.get.asInstanceOf[Traversable[Either[Throwable, Result[T]]]]
      }
    }
  }
}