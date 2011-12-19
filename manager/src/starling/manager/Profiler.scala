package starling.manager

import collection.mutable.{ArrayStack}

object TimeTree {
  def create(name:String, children:Iterable[TimeTree], time:Long) = {
    val remaining = if (children.nonEmpty) {
      val sum = children.map(_.time).sum
      List( TimeTree("Self", Nil, time-sum))
    } else { Nil }
    TimeTree(name, children.toList ::: remaining, time)
  }
}
case class TimeTree(val name:String, children:List[TimeTree], time:Long) {
  def text(padding:String=""):String = {
    val l = children.toList
    padding + name + " => (" + time + "ms)\n" + l.map(_.text(padding + "    ")).mkString
  }
}

class MutableTimeTree(val name:String) extends Serializable {
  val children = new ArrayStack[TimeTree]()
  def close(time:Long) = TimeTree.create(name, children.toList.reverse, time)
  def add(tree:TimeTree) {
    children.push(tree)
  }
  override def toString = name+":"+children.toString()
}

object Profiler {

  private val threadLocalTimeTree = new ThreadLocal[MutableTimeTree]() {
    override def initialValue() = new MutableTimeTree("un named")
  }

  def captureTime[T](name:String)(f: => T):(T,TimeTree) = {
    val start = System.currentTimeMillis()
    val current = threadLocalTimeTree.get
    val newStack = new MutableTimeTree(name)
    threadLocalTimeTree.set(newStack)
    val result = f;
    val timeTree = newStack.close(System.currentTimeMillis() - start)
    threadLocalTimeTree.set(current)
    current.add(timeTree)
    (result, timeTree)
  }

  def time[T](message: String)(f: => T) = {
    val (r, _) = captureTime(message) { f }
    r
  }

  def add(tree:TimeTree) {
    threadLocalTimeTree.get.add(tree)
  }
}