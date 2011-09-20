package starling.market

import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.calendar.BusinessCalendars
import starling.daterange._

class FuturesExpiryRulesImpl(eai: DB, businessCalendars: BusinessCalendars) extends FuturesExpiryRules(businessCalendars) {
  def ruleOption(eaiQuoteID: Int) = dbRules.get(eaiQuoteID)

  lazy private val dbRules: Map[Int, FuturesExpiryRule] = {
    var dates = Map.empty[Int, FuturesExpiryRuleMap]

    val q = (select("quoteid, expirytype,  month, year, expiryType, expirydate")
            from ("tblExpiryDates")
            where ("expiryType" in List(1,2,3)))
    eai.query(q) {
      rs => {
        val q = rs.getInt("quoteid")
        val m = rs.getInt("Month")
        val y = rs.getInt("Year")
        val month = Month(y, m)
        val day = rs.getDay("ExpiryDate")

        dates = dates.get(q) match {
          case Some(f) => {
            // expiryType: 1 -> Future, 2 -> Option, 3 Calendar Spread Option (CSO)
            val value = rs.getInt("expirytype") match {
              case 1 => f.addLastTradingDay(month, day)
              case 2 => f.addExpiryDay(month, day)
              case 3 => f.addCSOExpiryDay(month, day)
            }
            dates ++ Map(q -> value)
          }
          case None => {
            // expiryType: 1 -> Future, 2 -> Option, 3 Calendar Spread Option (CSO)
            rs.getInt("expirytype") match {
              case 1 => dates ++ Map(q -> FuturesExpiryRuleMap(q, lastTradingDays = Map(month -> day)))
              case 2 => dates ++ Map(q -> FuturesExpiryRuleMap(q, expiryDays = Map(month -> day)))
              case 3 => dates ++ Map(q -> FuturesExpiryRuleMap(q, csoExpiryDays = Map(month -> day)))
            }
          }
        }
      }
    }
    dates
  }
}

case class FuturesExpiryRuleMap(id: Int, lastTradingDays: Map[DateRange, Day] = Map(), expiryDays: Map[DateRange, Day] = Map(),
                                csoExpiryDays: Map[DateRange, Day] = Map()) extends FuturesExpiryRule {

  val name = "EAIExpiryRule(" + id + ")"

  def addLastTradingDay(dr: DateRange, lastTradingDay: Day): FuturesExpiryRuleMap = copy(lastTradingDays = lastTradingDays ++ Map(dr -> lastTradingDay))
  def addExpiryDay(dr: DateRange, lastTradingDay: Day): FuturesExpiryRuleMap = copy(expiryDays = expiryDays ++ Map(dr -> lastTradingDay))
  def addCSOExpiryDay(dr: DateRange, lastTradingDay: Day): FuturesExpiryRuleMap = copy(csoExpiryDays = csoExpiryDays ++ Map(dr -> lastTradingDay))

  override def lastTradingDay(d: DateRange) = lastTradingDays.get(d) match {
    case Some(day) => day
    case None => throw new NoExpiryDataException(d)
  }

  override def expiryDay(d: DateRange) = expiryDays.get(d) match {
    case Some(day) => day
    case None => throw new NoExpiryDataException(d)
  }

  override def csoExpiryDay(s: Spread[_ <: DateRange]) = csoExpiryDays.get(s.first) match {
    case Some(day) => day
    case None => throw new NoExpiryDataException(s.first)
  }
}