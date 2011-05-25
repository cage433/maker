package starling.gui.utils

import swing.{Reactor, Publisher}
import starling.utils.ImplicitConversions._

object RichReactor {
  implicit def enrichReactor(reactor : Reactor) = new RichReactor(reactor)
  implicit def enrichPublisherReactor[PR <: Reactor with Publisher](reactor: PR) = new RichReactor(reactor) {
    def suppressingSelf[T](action : => T) = suppressing(reactor)(action)
  }

  class RichReactor[R <: Reactor] (reactor: R) {
    def suppressing[T](ps: Publisher*)(action : => T) {
      ps.foreach(reactor.deafTo(_))

      val result = action

      ps.reverse.foreach(reactor.listenTo(_))

      result
    }
  }
}