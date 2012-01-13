package starling.local

import java.lang.ThreadLocal
import collection.immutable.Map

object Locals {
  private val threadLocals = new ThreadLocal[Map[Any, Any]]()

  def setLocal[T](name:String, value:Option[T]) = {
    val map:Map[Any, Any] = threadLocals.get match {
      case null => Map.empty
      case m => m
    }
    val updated = value match {
      case None => map - name
      case Some(v) => map + (name -> v)
    }
    threadLocals.set(updated)
  }

  def getLocal[T](name: String): Option[T] = Option(threadLocals.get()).flatMap(m => m.get(name).asInstanceOf[Option[T]])

  def withLocal[T, S](name:String, value:T)(f: => S):S = {
    val old:Map[Any, Any] = threadLocals.get match {
      case null => Map.empty
      case m => m
    }
    try {
      val updated = old + (name -> value)
      threadLocals.set(updated)
      f
    } finally {
      threadLocals.set(old)
    }
  }

  def withLocals[S](locals:Map[Any, Any])(f: => S):S = {
    val old = threadLocals.get
    try {
      threadLocals.set(locals)
      f
    } finally {
      threadLocals.set(old)
    }
  }

  def currentLocals:Map[Any, Any] = Option(threadLocals.get()).getOrElse(Map.empty)
}