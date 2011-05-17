package starling.calendar

import java.lang.String
import starling.db.DB
import scala.collection.mutable
import starling.utils.sql.QueryBuilder._
import starling.daterange.{Location, Day}


class DBHolidayTables(db : DB) extends HolidayTables{
  def financialHolidays(name: String) = BusinessCalendarSet(name, Location.Unknown, financialHolMap(name))
  def regionHolidays(name: String) = BusinessCalendarSet(name, Location.Unknown, regionHolMap(name))

  lazy val regionHolMap : Map[String, Set[Day]] = {
    val table = mutable.Map.empty[String, Set[Day]]
    //val db = new DB(Props.VarSqlServer.url, Props.VarSqlServer().dataSource)
    val q = (select ("region, date")
            from ("tblHolidays h")
            innerJoin ("tblCalendars c", ("c.id" eql "h.CalendarTypeID"))
            where ("date" gt Day(2005, 1, 1))
            )
    db.query(q){
      rs => {
        val region = rs.getString("Region")
        val day = rs.getDay("Date")
        table(region) = table.getOrElse(region, Set.empty[Day]) + day
      }
    }
    Map.empty ++ table
  }

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
