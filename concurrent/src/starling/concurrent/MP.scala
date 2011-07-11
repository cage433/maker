package starling.concurrent

import akka.dispatch.{Future, Futures}
import collection.generic.{TraversableFactory, GenericTraversableTemplate, CanBuildFrom}
import collection.mutable.ListBuffer
import starling.utils.{Stopwatch, Log}
import collection.{Traversable, TraversableLike}

class MP[A](val t: Traversable[A]) extends Traversable[A] with GenericTraversableTemplate[A, MP] with TraversableLike[A, MP[A]] {

  class UnitOfWork[B](val id: Long, f: A => B, work: => A) extends Work[B] {
    def doWork = {
      val res = f(work)
      Result(id, res)
    }
  }

  override def companion = MP

  override def foreach[U](f: A => U) {
    t.foreach(f)
  }

  def mpMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[MP[A], B, That]) = {
    val b = bf(repr)
    b.sizeHint(this.t)
    val results = process(f)
    results.map{
      case r: Result[_] => b += r.result.asInstanceOf[B]
    }
    b.result
  }

  def mpFlatMap[B, That](f: (A) => Traversable[B])(implicit bf: CanBuildFrom[MP[A], B, That]) = {
    val results = process(f)
    val b = bf(repr)
    results.map{
      case r: Result[_] => b ++= r.result.asInstanceOf[TraversableOnce[B]]
    }
    b.result
  }

  def fixStackTrace(e: Throwable, originalStacktrace: Array[StackTraceElement]): Throwable = {
    val index = originalStacktrace.indexWhere(e => e.toString.contains("UnitOfWork.doWork"))
    e.setStackTrace(originalStacktrace.slice(0, index) ++ Thread.currentThread.getStackTrace.drop(1))
    e
  }

  private def process[B](f: (A) => B) = {
    if (MP.disableMP) {
      t.toIterable.zipWithIndex.map{
        case (w, i) => Result(i, f(w))
      }
    } else {
      val work = t.toIterable.zipWithIndex.map{
        case (w, i) => new UnitOfWork(i, f, w)
      }
      val dispatcher = MP.dispatcher

      val results = dispatcher.dispatcher(work).map{
        case Left(e: Throwable) => {
          val exceptionStackTrace = e.getStackTrace
          val thisThreadsStackTrace = Thread.currentThread.getStackTrace
          throw fixStackTrace(e, exceptionStackTrace)
        }
        case Right(a) => a
      }

      results.toSeq.sortWith{
        case ((r1: Result[_]), (r2: Result[_])) => r1.id < r2.id
      }
    }
  }
}

object MP extends TraversableFactory[MP] {
  // Just in case you want to disable concurrent running of jobs, set this to true
  val disableMP = false

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MP[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A] = new ListBuffer[A] mapResult (x => new MP(x))

  implicit def iterToMPIter[A](t: Traversable[A]) = new MP(t)

  implicit def mpToList[A](mp: MP[A]): List[A] = List.empty ++ mp.t

  implicit def mpToMap[A, B](mp: MP[(A, B)]) = Map.empty ++ mp.t

  private var maybeDispatcher:Option[ActorSupervisor] = None

  def dispatcher:ActorSupervisor = {
    this synchronized {
      maybeDispatcher match {
        case None => {
          val d = new ActorSupervisor
          d.start
          maybeDispatcher = Some(d)
          d
        }
        case Some(d) => d
      }
    }
  }

  def stop = {
    this synchronized {
      maybeDispatcher.map(_.stop)
      maybeDispatcher = None
    }
  }
}
