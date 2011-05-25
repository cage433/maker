package starling.calendar

import starling.daterange.Day

object Holiday {
  def isNewYearsEve(day: Day) = {
    (day.month == 12 && day.dayNumber == 31)
  }

  def isNewYearsDay(day: Day) = {
    (day.month == 1 && day.dayNumber == 1)
  }

  def isHolyThursday(day: Day) = {
    val easter = ReligiousCalendar.catholicEaster(day.year)
    day.equals(easter - 3)
  }

  def isGoodFriday(day: Day) = {
    val easter = ReligiousCalendar.catholicEaster(day.year)
    day.equals(easter - 2)
  }

  def isEasterMonday(day: Day) = {
    val easter = ReligiousCalendar.catholicEaster(day.year)
    day.equals(easter + 1)
  }

  /**
   * Labour day for most countries except US
   *
   * @see isUSLabourDay
   */
  def isLabourDay(day: Day) = {
    (day.month == 5 && day.dayNumber == 1)
  }

  /**
   * Labour day only for US
   */
  def isUSLabourDay(day: Day) = {
    (day.month == 9 && day.dayNumber == 1)
  }

  def isAscensionThursday(day: Day) = {
    val easter = ReligiousCalendar.catholicEaster(day.year)
    day.equals(easter + 39)
  }

  def isWhitMonday(day: Day) = {
    val easter = ReligiousCalendar.catholicEaster(day.year)
    day.equals(easter + 50)
  }

  def isChristmasEve(day: Day) = {
    (day.month == 12 && day.dayNumber == 24)
  }

  def isChristmasDay(day: Day) = {
    (day.month == 12 && day.dayNumber == 25)
  }

  def isBoxingDay(day: Day) = {
    (day.month == 12 && day.dayNumber == 26)
  }

  /**
   * @Same as #isSpringBankHoliday
   */
  def isMemorialDay(day: Day) = {
    isSpringBankHoliday(day)
  }

  def isIndependenceDay(day: Day) = {
    (day.month == 7 && day.dayNumber == 4)
  }

  /**
   * 4th Thursday in November
   */
  def isThanksgiving(day: Day) = {
    var result = false
    if (day.month == 11 && day.isThursday) {
      var previousThursdays = 0
      for (i <- 1 until day.dayNumber) {
        previousThursdays += (if (Day(day.year, day.month, i).isThursday) 1 else 0)
      }
      result = previousThursdays == 3
    }
    result
  }

  /**
   * First Monday in May
   */
  def isEarlyMayBankHoliday(day: Day) = {
    var result = false
    if (day.month == 5 && day.isMonday) {
      result = true
      for (i <- 1 until day.dayNumber) {
        result = result && !Day(day.year, day.month, i).isMonday
      }
    }
    result
  }

  /**
   * Last Monday in May
   */
  def isSpringBankHoliday(day: Day) = {
    var result = false
    if (day.month == 5 && day.isMonday) {
      result = true
      val daysInMonth = day.containingMonth.days.size
      for (i <- (daysInMonth until day.dayNumber)) {
        result = result && !Day(day.year, day.month, i).isMonday
      }
    }
    result
  }

  /**
   * Last Monday in August
   */
  def isSummerBankHoliday(day: Day) = {
    var result = false
    if (day.month == 8 && day.isMonday) {
      result = true
      val daysInMonth = day.containingMonth.days.size
      for (i <- (daysInMonth until day.dayNumber)) {
        result = result && !Day(day.year, day.month, i).isMonday
      }
    }
    result
  }
}
