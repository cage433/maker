package starling.utils

trait Startable {
  def start : Unit
}

object Startable {
  val NULL = new Startable { def start = {} }
}

trait Stoppable extends Startable {
  def stop : Unit

  def close() = stop
}

object Stoppable {
  val NULL = new Stoppable { def stop = {}; def start = {} }

  def createIf(condition : => Boolean)(creator : => Stoppable) : Stoppable = {
    if (condition) {
      creator
    }
    else {
      NULL
    }
  }
}