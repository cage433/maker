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

import java.io.File
import os.Command
import os.OsUtils._
import java.io.PrintWriter
import java.io.StringWriter
import maker.MakerProps

object Utils {
  def fix[A, B](f: (A => B) => (A => B)): A => B = f(fix(f))(_)

  /** this assumes gnome for linux
   *  can add cmd line view (lynx) and WM detection later
   */
  def openHtmlFile(props : MakerProps, f : File) = {
    if (isLinux)
      Command(props.log, "gnome-open", f.getAbsolutePath).execAsync
    else // assume OSX until we want to support other OSes such as windows
      Command(props.log, "open", f.getAbsolutePath).exec()
    f
  }

  def showImage(props : MakerProps, f : File) = {
    if (isLinux)
      Command(props.log, "xdg-open", f.getAbsolutePath).execAsync
    else // assume OSX until we want to support other OSes such as windows
      Command(props.log, "open", f.getAbsolutePath).exec()
    f
  }
  def stackTraceAsString(t : Throwable) = {
    val sw = new StringWriter()
    val w = new PrintWriter(sw)
    t.printStackTrace(w)
    sw.toString
  }

  def time(closure : () => AnyRef, msg : String = "") {
    val sw = maker.utils.Stopwatch()
    closure()
    println(msg + " took " + sw.ms())
  }

  val sysProps = {
    import scala.collection.JavaConversions._
    Map() ++ mapAsScalaMap(System.getProperties.asInstanceOf[java.util.Map[String, String]])
  }
  def subsWithSysProps(value : String)  = subs(sysProps)(value  )

  def subs(props : Map[String, String])(value : String)  = {
    val start = value.indexOf("${")
    val end = value.indexOf("}")
    if (start >= 0 && start < end) {
      val key = value.substring(start + 2, end)
      props.get(key) match {
        case None => value
        case Some(v) => value.substring(0, start) + v + value.substring(end + 1, value.length)
      }
    }
    else value
  }
}
