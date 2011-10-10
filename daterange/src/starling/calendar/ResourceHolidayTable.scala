package starling.calendar


import starling.daterange.Day
import java.util.StringTokenizer
import starling.utils.StringIO

object ResourceHolidayTable {
  def apply(calendar: String): Set[Day] = {
    var str = StringIO.readStringFromResource(getClass, "/calendar/" + calendar);
    Set[Day]() ++ (for(line <- str.split('\n') if !line.startsWith("#")) yield {
      Day.parse(line.stripLineEnd.trim)
    })
  }
}
