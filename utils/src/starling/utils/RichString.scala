package starling.utils

import java.net.{URLDecoder, URLEncoder}

import starling.utils.Pattern._
import scalaz.Scalaz._
import collection.Seq
import java.security.MessageDigest
import starling.utils.ImplicitConversions._


trait RichString {
	implicit def StringToRichString(s : String) = new RichString(s)

  def box(args: Seq[Any]): Array[AnyRef] = args.flatMap(boxValue).toArray
  def nullsafe(args: Seq[Any]): Seq[Any] = args.map(_ ?? "null")

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
      case x: (Any, Any) => boxValue(x._1) ++ boxValue(x._2)
      case x: Unit => "()" :: Nil
      case x: AnyRef => x :: Nil
    }
  }

	class RichString(s: String){
		def % (args: Any*) = String.format(s, box(nullsafe(args)):_*)

		def join(elems: Iterable[Any]): String = join(elems.iterator)
		def join(elems: Iterator[Any]): String = {
			elems.foldLeft("")((s1, s2) => s1 match {
			case "" => s2.toString
			case _  => s1.toString + s + s2.toString
			})
		}

    def removeWhiteSpace = {
      val buffer = new StringBuilder
      for (c <- s if !Character.isWhitespace(c)) buffer.append(c)
      buffer.toString
    }

    def urlEncode = URLEncoder.encode(s, "UTF-8")
    def urlDecode = URLDecoder.decode(s, "UTF-8")
    def withIndex(index: Int) = new IndexedString(s, index)
    def replaceLast(searchFor: String, replaceWith: String) = {
      val pos = s.lastIndexOf(searchFor)
      if (pos == -1) s else s.substring(0, pos) + replaceWith + s.substring(pos + searchFor.size)
    }

    def isInt = try {
      s.toInt
      true
    }
    catch {
      case n: NumberFormatException => false
    }
    
    def containsOneOf(searchFor: String*) = searchFor.exists(s.contains(_))
    def emptyTo(alternative: String) = fold(identity, alternative)
    def uncapitalize: String = fold(_.charAt(0).toLower + s.substring(1))
    def hyphenate: String = splitByCase("-").toLowerCase
    def unhyphenate: String = s.split("-").map(_.capitalize).mkString("")
    def strip(toRemove: String*): String = (s /: toRemove)((acc, itemToRemove) => acc.replaceAll(itemToRemove, ""))
    def prefixWith(prefix: String) = if (s.startsWith(prefix)) s else prefix + s
    def trimHereDoc: String = s.split("\n").map(_.trim).mkString("\n")
    def splitByCase(sep: String = " ") = fold(_.replaceAll(String.format("%s|%s|%s",
         "(?<=[A-Z])(?=[A-Z][a-z])", "(?<=[^A-Z])(?=[A-Z])", "(?<=[A-Za-z])(?=[^A-Za-z])"), sep))
    def md5: String = {
      val md5 = MessageDigest.getInstance("MD5").updateIt(_.reset(), _.update(s.getBytes))

      md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }


    private def fold(nonEmpty: (String) => String, empty: String = s) = if (s == null || s.isEmpty) empty else nonEmpty(s)
	}
}
