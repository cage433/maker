package maker.utils

import scala.collection.TraversableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.TreeMap
import scalaz.syntax.id._
import java.io.StringWriter
import java.io.PrintWriter

object Implicits{

  object RichTraversableLike{
    class RichTraversableLike[+A, +Repr](tl : TraversableLike[A, Repr]) {
      def \ (x : Any) : Repr = tl.filterNot(_ == x)
      def \\ (seq : SeqLike[_, _]) = tl.filterNot(seq.contains(_))
      def quote(implicit bf: CanBuildFrom[Repr, String, Traversable[String]]) = tl.map(element => "'" + element + "'")

      /** Zip every element if the TraversableLike with a constant element 'b'*/
      def zipWith[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, (A, B), That]): That = tl.map(a => (a, f(a)))
      /** Pair every element if the TraversableLike with a constant element 'b'*/
      def pairWith[B, That](b: B)(implicit cbf: CanBuildFrom[Repr, (A, B), That]): That = tl.map(_ -> b)

      def initOption(): Option[Repr] = if (tl.isEmpty) None else Some(tl.init)

      /** Apply the function 'f' if the TraversableLike is nonEmpty */
      //def ifDefined[B](f: Repr => B): Option[B] = tl.isEmpty ? none[B] | some(f(tl.repr))

      def partitionEithers[B, C, LThat, RThat](f: A => Either[B, C])(
        implicit cbl: CanBuildFrom[Repr, B, LThat], cbr: CanBuildFrom[Repr, C, RThat]): (LThat, RThat) = { 

        val (lbuilder, rbuilder) = (cbl(tl.repr), cbr(tl.repr))

        tl.foreach(f(_).fold(lbuilder += _, rbuilder += _)) 

        (lbuilder.result(), rbuilder.result())
      }   
    }
    implicit def traversableLike2RichTraversableLike[A, Repr](tl: TraversableLike[A, Repr]) = new RichTraversableLike(tl)
  }   

  object RichIterable{

    implicit def toRichIterable[A](as : Iterable[A]) = RichIterable(as)
    case class RichIterable[A](as : Iterable[A]){
      def filterOnType[T]()(implicit m : Manifest[T]) = {
        as.filter(m.erasure.isInstance(_)).map(_.asInstanceOf[T])
      }
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
    }
  }


  object RichString {
    implicit def StringToRichString(s : String) = new RichString(s)
    implicit def StringBufferTorichStringBuffer(b : StringBuffer) = new RichStringBuffer(b)

    def box(args: Seq[Any]): Array[AnyRef] = args.flatMap(boxValue).toArray
    def nullsafe(args: Seq[Any]): Seq[Any] = args.map(_ ?? "null")

    def apply(s : String) = RichString(s)
    private def boxValue(x: Any): Seq[AnyRef] = {
      if (x == null) {
        null
      }
      else x match {
        case x: Boolean => new java.lang.Boolean(x) :: Nil
        case x: Byte => new java.lang.Byte(x) :: Nil
        case x: Short => new java.lang.Short(x) :: Nil
        case x: Char => new java.lang.Character(x) :: Nil
        case x: Int => new java.lang.Integer(x) :: Nil
        case x: Long => new java.lang.Long(x) :: Nil
        case x: Float => new java.lang.Float(x) :: Nil
        case x: Double => new java.lang.Double(x) :: Nil
        // TODO: [17 Jan 2012]: Replace with generic implementation x.productIterator.toSeq.flatMap(boxValue)
        case x: (Any, Any) => boxValue(x._1) ++ boxValue(x._2)
        case x: Unit => "()" :: Nil
        case x: AnyRef => x :: Nil
      }
    }

    class RichStringBuffer(b : StringBuffer){
      def addLine(text : AnyRef){
        if (text.toString.endsWith("\n"))
          b.append(text)
        else
          b.append(text + "\n")
      }
    }
    case class RichString(s: String){
      def % (args: Any*) = String.format(s, box(nullsafe(args)):_*)
      def inGreen = "\033[0;32m" + s + "\033[0m"
      def inRed = "\033[1;31m" + s + "\033[0m"
      def inReverseRed = "\033[7;31m" + s + "\033[0m"
      def inBlue = "\033[1;34m" + s + "\033[0m"
      def justified : String = {
        val n = 2
        if (n == 0 || s.length % n == 0)
          s
        else {
          s + " " * (n - (s.length % n))
        }
      }
      def formatted(indentSize : Int) = {
        if (s == "")
          2 *indentSize + s.justified
        s.split("\n").toList.map(
          2 *indentSize + _.justified
        ).mkString("\n")
      }
      def indent(width : Int) : String = indent(" " * width)
      def indent(ind : String) : String = s.split("\n").mkString(ind, "\n" + ind, "\n")
      def padLeft(n : Int) : String = {
        if (s.length < n)
          " " * (n - s.length) + s
        else
          s
      }
      def padRight(n : Int) : String = {
        if (s.length < n)
          s + " " * (n - s.length) 
        else
          s
      }
    }
  }

  object RichThrowable{
    implicit def throwable2RichThrowable(th : Throwable) = RichThrowable(th)
    case class RichThrowable(th : Throwable){
      def stackTraceAsString = {
        val sw = new StringWriter()
        val w = new PrintWriter(sw)
        th.printStackTrace(w)
        sw.toString
      }
    }
  }

}
