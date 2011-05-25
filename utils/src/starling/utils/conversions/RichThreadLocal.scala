package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichThreadLocal {
  implicit def enrichThreadLocal[T](threadLocal: ThreadLocal[T]) = new RichThreadLocal(threadLocal)

  class RichThreadLocal[T](threadLocal: ThreadLocal[T]) {
    def pop(): T = threadLocal.get.update(value => threadLocal.remove)
    def set(option: Option[T]) = option match {
      case Some(value) => threadLocal.set(value)
      case None => threadLocal.remove
    }
  }
}