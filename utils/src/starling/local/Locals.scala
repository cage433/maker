package starling.local

import concurrent.stm._
import java.lang.{IllegalStateException, ThreadLocal}
import collection.immutable.{List, Map}

/**
 * A value and possibly a version - the version is an arbitrary int that must
 * change if the value is different from another local under the same name
 *
 * A Local with a version, once pushed on the stack, cannot change further down
 * the stack. If it doesn't have a version then it can.
 */
case class Local[T](value:T, version: Option[Int]) {
  def static = version.isDefined
}

case class Accessed(name: String, version: Option[Int])

object Locals {
  private val threadLocals = new InheritableThreadLocal[Map[Any, Local[_]]]()

  private val CallbackString = "Callback"
  private case class Callback(f:(Accessed) => Unit)

  /**
   * Don't record that we looked for this local - good for things like logging where
   * looking at the local hasn't changed the outcome of what is processing.
   */
  def peekLocal[T](name: String) = getLocal(name, peek = true)

  /**
   * Returns the local if it exists - also records that this local was
   * accessed which may be used to invalidate a cache. If you don't want
   * this behaviour use with peek = true
   */
  def getLocal[T](name:String, peek: Boolean = false):Option[T] = {
    val option = Option(threadLocals.get())
    option.flatMap(m => {
      m.get(name) match {
        case Some(local) => {
          if (!peek) {
            m.get(CallbackString).map {
              case Local(callbacks:List[Callback], _) => {
                callbacks.map(_.f(Accessed(name, local.version)))
              }
            }
          }
          Some(local.value)
        }
        case _ => None
      }
    }).asInstanceOf[Option[T]]
  }

  /**
   * Push local onto Stack - it will be available in all threads created or used from
   * while 'f' is executing.
   */
  def withLocal[T, S](name:String, value: T)(f: => S):S = {
    val old:Map[Any, Local[_]] = threadLocals.get match {
      case null => Map.empty
      case m => m
    }
    try {
      val updated = old + (name -> Local(value, None))
      threadLocals.set(updated)
      f
    } finally {
      threadLocals.set(old)
    }
  }

  /**
   * Push local onto Stack - it will be available in all threads created or used from
   * while 'f' is executing.
   * The local cannot be changed further down the Stack.
   */
  def withStaticLocal[T, S](name:String, value:Local[T])(f: => S):S = {
    val old:Map[Any, Local[_]] = threadLocals.get match {
      case null => Map.empty
      case m => m
    }
    try {
      if(old.get(name).nonEmpty){
        throw new IllegalStateException("Trying to set another Static Local when one is already defined: " + (old.get(name), value))
      }
      val updated = old + (name -> value)
      threadLocals.set(updated)
      f
    } finally {
      threadLocals.set(old)
    }
  }

  /**
   * Record which locals are accessed by getLocal (above) while 'f' is running.
   */
  def recordAccessedLocals[S](f: => S): (S, Set[Accessed]) = {
    val accessed = TSet[Accessed]()
    def record(a:Accessed):Unit = atomic {
      implicit txn => {
        accessed += a
      }
    }

    val cbs:List[_] = peekLocal(CallbackString) match {
      case Some(callbacks) => {
        Callback(record) :: callbacks.asInstanceOf[List[Callback]]
      }
      case _ => {
        List(Callback(record))
      }
    }

    val result = withLocal (CallbackString, cbs) {
      f
    }
    val recorded = accessed.single.toSet
    (result, recorded)
  }

  /**
   * Creates a blank Local stack to run in, then restores it once finished running 'f'
   *
   * Used for testing - not a good idea to use outside of tests
   */
  def newStack[S](f: => S): S = withLocals(Map.empty)(f)

  /**
   * Be careful using this, it will override all locals
   */
  def withLocals[S](locals:Map[Any, Local[_]])(f: => S):S = {
    val old = threadLocals.get
    try {
      threadLocals.set(locals)
      f
    } finally {
      threadLocals.set(old)
    }
  }

  /**
   * Be careful if you use this - it doesn't record access by design but this
   * may break your code (e.g. it could break caching)
   */
  def currentLocals:Map[Any, Local[_]] = Option(threadLocals.get()).getOrElse(Map.empty)
}