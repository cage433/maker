package scala {
  package object concurrent {
    val ExecutionContexts = akka.dispatch.ExecutionContexts
    type ExecutionContext = akka.dispatch.ExecutionContext
    val ExecutionContext = akka.dispatch.ExecutionContext
    type Future[T] = akka.dispatch.Future[T]
    val Future = akka.dispatch.Future
    type Promise[T] = akka.dispatch.Promise[T]
    val Promise = akka.dispatch.Promise
    val Await = akka.dispatch.Await

    private val uncaught: Thread.UncaughtExceptionHandler = new Thread.UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable): Unit = {
        System.err.println("uncaught exception in " + t.getName)
        e.printStackTrace
      }
    }
    import java.util.concurrent.ForkJoinPool
    implicit val ExecutionContext_Implicits_global: ExecutionContext =
      akka.dispatch.ExecutionContexts.fromExecutor(new ForkJoinPool(
        Runtime.getRuntime.availableProcessors,
        ForkJoinPool.defaultForkJoinWorkerThreadFactory,
        uncaught, true))
  }

  package concurrent {
    package object duration {
      type Duration = akka.util.Duration
      val Duration = akka.util.Duration
      implicit def intToDuration(n: Int) = akka.util.duration.intToDurationInt(n)
      implicit def longToDuration(n: Long) = akka.util.duration.longToDurationLong(n)
    }
  }

  package util {
    object Try {
      def apply[T](block: => T): Try[T] = {
        try {
          Success(block)
        } catch {
          case throwable: Throwable => Failure(throwable)
        }
      }
    }

    sealed trait Try[+T] {
      def get: T
      def toOption:Option[T]
      def isFailure: Boolean
      def isSuccess: Boolean
    }

    case class Success[T](result: T) extends Try[T] {
      override def get = result
      override def toOption = Some(result)
      override val isFailure: Boolean = false
      override val isSuccess: Boolean = true
    }

    case class Failure(throwable: Throwable) extends Try[Nothing] {
      override def get = throw throwable
      override def toOption = None
      override val isFailure: Boolean = true
      override val isSuccess: Boolean = false
    }
  }
}
