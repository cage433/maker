package maker.utils

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object RichIterable{
  implicit def toRichIterable[A](as : Iterable[A]) = RichIterable(as)
}
case class RichIterable[A](as : Iterable[A]){
  def indented(indent : Int = 1) = as.mkString("\n" + "\t" * indent, "\n" + "\t" * indent, "")
  def formatted(length : Int = 3) : String = {
    if (as.size <= length)
      as.mkString(", ")
    else 
      as.take(length).mkString(", ") + ", ..."
  }

  def asTable(width : Int) : String = {
    def transpose[A](xs : List[List[A]]) : List[List[A]] = xs.filter(_.nonEmpty) match {
      case Nil => Nil
      case ys => ys.map(_.head) :: transpose(ys.map(_.tail))
    }
    val lists : List[List[String]] = as.map(_.toString).toList.grouped(width).toList
    val columnWidths = transpose(lists).map{l => val cw = l.map(_.length).max; cw + (cw & 1)} // width to even column - hack to avoid using \t
    val justified : List[List[String]] = lists.map(_.zip(columnWidths).map{
      case (s, cw) => s + " " * (cw - s.length)
    })
    justified.map(_.mkString("  ")).mkString("\n")
  }

  def inAlphabeticalOrder = as.toList.sortWith(_.toString < _.toString)
  def closest(metric : A => Int) : Set[A] = {
    if (as.isEmpty){
      Set[A]()
    } else {
      val map = TreeMap[Int, Set[A]]() ++ as.toSet.groupBy(metric)
      map.head._2
    }
  }

  def uniqueElement : A = {
    as.toList match{
      case List(a) => a
      case _ => throw new RuntimeException("Should only have one element, has " + as)
    }
  }

  def distinctBy[That](f: (A, A) => Boolean): List[A] = {
    var seen = Set[A]()
    var builder = new ListBuffer[A]()
    as.foreach { a =>
      if (!seen.exists(s => f(a, s))) {
        seen += a
        builder += a
      }
    }
    builder.result()
  }

}
