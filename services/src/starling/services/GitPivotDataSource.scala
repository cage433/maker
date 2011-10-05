package starling.services

import starling.pivot._
import org.apache.commons.io.IOUtils
import starling.daterange.Timestamp
import org.joda.time.format.DateTimeFormat
import starling.pivot.utils.PeriodPivotFormatter

class GitPivotDataSource(numCommits:Int) extends UnfilteredPivotTableDataSource {
  def fieldDetailsGroups:List[FieldDetailsGroup] = {
    val fd = List(
      FieldDetails("Commit ID"),
      FieldDetails("Author"),
      new FieldDetails("Date") {
        override def formatter:PivotFormatter = new PivotFormatter {
          def format(value:Any, formatInfo:ExtraFormatInfo):TableCell = {
            value match {
              case t:Timestamp => {
                val monthText = PeriodPivotFormatter.format(t.month, formatInfo).text
                val dayString = {
                  val zz = t.day.dayNumber.toString
                  if (Character.isDigit(monthText(0))) {
                    zz + "/"
                  } else {
                    zz
                  }
                }
                val textToUse = dayString + monthText + " " + t.timeStringWithSeconds
                new TableCell(value, textToUse)
              }
              case _ => new TableCell(value)
            }
          }
        }
      },
      new FieldDetails("Message") {
        override def isDataField = true
      }
    )
    List(FieldDetailsGroup("Git Fields", fd))
  }
  def unfilteredData(pfs:PivotFieldsState):List[Map[Field,Any]] = {
    val process = Runtime.getRuntime.exec("git log -" + numCommits.toString)
    val stdOut = IOUtils.toByteArray(process.getInputStream)
    IOUtils.toByteArray(process.getErrorStream)
    process.waitFor
    if (process.exitValue == 0) {
      val fmt = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss yyyy Z")
      val lines = new String(stdOut).split("\n")
      var lineCount = 0
      (0 until numCommits).map(c => {
        var innerLineCount = 0
        val commitID = lines(lineCount + innerLineCount).substring(7)
        innerLineCount += 1
        val authorString = {
          val t = lines(lineCount + innerLineCount)
          if (t.startsWith("Merge:")) {
            innerLineCount += 1
            lines(lineCount + innerLineCount)
          } else {
            t
          }
        }
        val author = {
          val i = authorString.indexOf("<")
          authorString.substring(8, i-1)
        }
        innerLineCount += 1
        val date = Timestamp(fmt.parseDateTime(lines(lineCount + innerLineCount).substring(8)).getMillis)
        innerLineCount += 2
        val message = lines(lineCount + innerLineCount).substring(4).trim()
        val (_,rest) = lines.splitAt(lineCount + innerLineCount)
        rest.zipWithIndex.find{case (s,_) => s.startsWith("commit")} match {
          case None => lineCount += innerLineCount + 2
          case Some((_,i)) => {lineCount += (innerLineCount + i)}
        }
        Map(Field("Commit ID") -> commitID, Field("Author") -> author, Field("Date") -> date, Field("Message") -> message)
      }).toList
    } else {
      Nil
    }
  }
}