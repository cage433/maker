package starling.utils

import concurrent.SyncVar


class SynchronizedVar[A](initial: => A) extends SyncVar[A] {
  set(initial)

  def update(f: (A) => A) = synchronized { set(f(get)) }
}