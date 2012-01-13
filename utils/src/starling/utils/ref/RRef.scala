package starling.utils.ref

import util.DynamicVariable
import concurrent.stm._
import collection.immutable.Map
import java.util.concurrent.atomic.AtomicBoolean

case class RRef[T, ID](contextGroup:String, contextName:String, staticID:ID) {
  def apply():T = RRef.dereference(this)
}

object RRef {
  private var builder = List[(String, String, RRefContext[_])]()
  private val built = new AtomicBoolean(false)
  private lazy val groups:Map[String, Map[String, RRefContext[_]]] = {
    built.set(true)
    builder.groupBy(_._1).map {
      case (group, v) => {
        val map:Map[String, RRefContext[_]] = v.map(e => e._2 -> e._3).toMap
        group -> map
      }
    }
  }

  private[ref] def dereference[T, TID](rref:RRef[T, TID]):T = {
    groups.get(rref.contextGroup) match {
      case Some(names) => {
        names.get(rref.contextName) match {
          case Some(contexts) => {
            contexts.dereference(rref)
          }
        }
      }
    }
    null.asInstanceOf[T]
  }

  def registerRRefContext(contextGroup:String, context:RRefContext[_]) = builder.synchronized {
    assert(!built.get, "Already built")
    builder ::=(contextGroup, context.name, context)
  }

  def inCurrentContext[S](contextGroup:String)(thunk:() => S):S = {
    // verify & set up context
    val contexts = groups.getOrElse(contextGroup, throw new Exception("Invalid group: " + contextGroup))
    contexts.mapValues(_.snapshot())
    val res = thunk()
    // tear down context
    contexts.mapValues(_.teardown())
    res
  }
}

trait RRefContext[C] {
  private[ref] val contexts = new DynamicVariable[C](null.asInstanceOf[C])

  private[ref] def dereference[T, ID](rref:RRef[T, ID]):T = dereference(contexts.value, rref)

  private[ref] def snapshot() = synchronized {
    assert(contexts.value == null)
    contexts.value = snapshotOfCurrentContext
  }

  private[ref] def teardown() = synchronized {
    assert(contexts.value != null)
    contexts.value = null.asInstanceOf[C]
  }

  def name:String

  def snapshotOfCurrentContext[C]:C

  protected def dereference[T, ID](context:C, rref:RRef[T, ID]):T
}

object Tmp {
  def main (args:Array[String]) {

  }
}