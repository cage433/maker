package maker.utils

import java.io.File
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import os.Command
import os.OsUtils._
import java.io.PrintWriter
import java.io.StringWriter

object Utils {
  def fix[A, B](f: (A => B) => (A => B)): A => B = f(fix(f))(_)

  /** this assumes gnome for linux
   *  can add cmd line view (lynx) and WM detection later
   */
  def openHtmlFile(f : File) = {
    if (isLinux)
      Command("gnome-open", f.getAbsolutePath).execAsync
    else // assume OSX until we want to support other OSes such as windows
      Command("open", f.getAbsolutePath).exec()
    f
  }

  def showImage(f : File) = {
    if (isLinux)
      Command("xdg-open", f.getAbsolutePath).execAsync
    else // assume OSX until we want to support other OSes such as windows
      Command("open", f.getAbsolutePath).exec()
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
