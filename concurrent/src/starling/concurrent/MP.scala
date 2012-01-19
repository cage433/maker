package starling.concurrent

import collection.generic.{TraversableFactory, GenericTraversableTemplate, CanBuildFrom}
import collection.mutable.ListBuffer
import starling.utils.{Stopwatch, Log}
import collection.{GenTraversableOnce, Traversable, TraversableLike}
import collection.parallel.ParIterable
import starling.local.Locals

class MP[+A](val t:Traversable[A]) {
  def mpMap[S, That](f:A => S) = {
    val locals = Locals.currentLocals
    t.par.map {
      a => {
        Locals.withLocals(locals) {
          f(a)
        }
      }
    }
  }

  def mpFlatMap[S, That](f:A => GenTraversableOnce[S]) = {
    val locals = Locals.currentLocals
    t.par.flatMap {
      a => {
        Locals.withLocals(locals) {
          f(a)
        }
      }
    }
  }
}

object MP {
  implicit def iterToMPIter[A](t:Traversable[A]) = new MP(t)

  implicit def parToList[A](p:ParIterable[A]):List[A] = p.toList

  implicit def parToMap[A, B](p:ParIterable[(A, B)]) = p.toMap
}
