/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils

import scalaz.syntax.id._
import scala.collection.immutable.TreeMap
import java.io.StringWriter
import java.io.PrintWriter


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
    import ScreenUtils.EscapeChars

    private def inColourCode(code: String) = if (EscapeChars) "\033[" + code + "m" + s + "\033[0m" else s
    def inGreen = inColourCode("0;32")
    def inLightRed = inColourCode("1;31")
    def inRed = inColourCode("0;31")
    def inReverseRed = inColourCode("7;31")
    def inBlue = inColourCode("0;34")
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

    def truncate(n : Int) : String = {
      if (s.length > n)
        s.take(n - 3) + "..."
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
