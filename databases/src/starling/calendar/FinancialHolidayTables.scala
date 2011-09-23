package starling.calendar

import java.lang.String
import starling.db.DB
import scala.collection.mutable
import starling.dbx.QueryBuilder._
import starling.daterange.{Location, Day}


trait FinancialHolidayTables extends HolidayTables {
  val db: DB

  def financialHolidaysOption(name: String) = financialHolMap.get(name).map(BusinessCalendarSet(name, Location.Unknown, _))

  lazy val financialHolMap : Map[String, Set[Day]] = {
    val table = mutable.Map.empty[String, Set[Day]]
    //val db = new DB(Props.VarSqlServer.url, Props.VarSqlServer().dataSource)
    val q = (select ("CalendarCode, Date")
            from "tblFCHolidays h"
            innerJoin ("tblFCCalendars c", "h.FCCalendarID" eql "c.id")
            where ("date" gt Day(2005, 1, 1)))
    db.query(q){
      rs => {
        val code = rs.getString("CalendarCode")
        val day = rs.getDay("Date")
        table(code) = table.getOrElse(code, Set.empty[Day]) + day
      }
    }
    table.foreach {
      case (cal, days) => assert(days.nonEmpty, "No holidays for calendar " + cal)
    }
    Map.empty ++ table
  }
}
