package starling.utils

import ImplicitConversions._


object Utils {
  case object Linux
  trait Windows
  case object WindowsXP extends Windows
  case object Windows7 extends Windows
  case object WindowsUnknown extends Windows
  case class UnknownOS(osName: String)

  def os = {
    val osName = System.getProperty("os.name").toLowerCase
    if(osName.startsWith("linux")) {
      Linux
    } else if (osName.startsWith("windows xp")) {
      WindowsXP
    } else if (osName.startsWith("windows 7")) {
      Windows7
    } else if (osName.startsWith("windows")) {
      WindowsUnknown
    } else {
      UnknownOS(osName)
    }
  }


  /**
   * Retry to do `f` a number of `times`, catching all exceptions.
   * If it still fails after `times` attempts the last exception is thrown
   */
  def retry[T](times: Int)(f: => T): T = {
    assert(times > 0)

    var exception: Option[Throwable] = None
    var done = false
    var result: Option[T] = None
    for (i <- 1 to times if !done) try {
      result = Some(f)
      exception = None
      done = true
    } catch {
      case e: Throwable => exception = Some(e)
    }

    if (!done) {
      throw exception.get
    }
    result.get
  }

  val csvConverter = GridConverter(cellSep = ",",
    escaper = (item: String) => if (item.matches(".*(,|\n).*")) '\"' + item.replace("\"", "\"\"") + '\"' else item)

  val tableConverter = GridConverter(open = "<table border='1'>", close = "</table>", rowOpen = "<tr>", rowClose = "</tr>",
                                     cellOpen = "<td>", cellClose = "</td>", escaper = (item: String) => item.emptyTo("&nbsp;"))
}

case class GridConverter(open: String = "", close: String = "", rowOpen: String = "", rowClose: String = "", rowSep: String = "\n",
                         cellOpen: String = "", cellClose: String = "", cellSep: String = "",
                         escaper: String => String = identity) {
  def convert(grid: List[List[Any]]): String =
    grid.map(row => convertRow(row)).mkString(open + rowOpen, rowClose + rowSep + rowOpen, rowClose + close)
  def convertRow(row: List[Any]): String =
    row.map(x => escaper(x.toString)).mkString(cellOpen, cellClose + cellSep + cellOpen, cellClose)
}