package starling.calendar

import starling.daterange.Day

class BusinessCalendars(val holidayTables : HolidayTables) {
  // Countries
  lazy val UK = holidayTables.UK
  lazy val US = holidayTables.US

  // Exchanges
  lazy val LME = holidayTables.LME
  lazy val NYMEX = holidayTables.NYMEX
  lazy val COMEX = NYMEX // NYMEX and COMEX are the same
  lazy val SFS = holidayTables.SFE // Shanghai Futures Exchange trading holidays
  lazy val SFE = SFS // SFS is sometimes called SFE
  lazy val CHINESE_STATE_HOLIDAYS = SFE //I think SFE just follows the standard Chinese holidays
  lazy val BALTIC = holidayTables.BALTIC
  lazy val IPE = holidayTables.ICE // ICE Futures Europe trading holidays (Crude and Refined contracts)
  lazy val ICE = IPE
  lazy val KLS = holidayTables.KLS
  lazy val ARE = holidayTables.financialHolidays("ARE") // Argus European Products Report non-publication days

  lazy val PLD = holidayTables.financialHolidays("PLD") // Platts Asia Pacific/Arab Gulf Marketscan non-publication days
  lazy val PLE = holidayTables.financialHolidays("PLE") // Platts European Marketscan non-publication days
  lazy val PLH = holidayTables.financialHolidays("PLH") // Platts US Marketscan non-publication days
  lazy val PLATTS_EUROPEAN_CRUDE = holidayTables.financialHolidays("Platts European Crude")

  lazy val LBMA = holidayTables.LBMA

  lazy val calendars = List(US, UK, ARE, KLS, LME, NYMEX, COMEX, SFS, SFE, BALTIC, ICE, PLD, PLE, PLH, PLATTS_EUROPEAN_CRUDE, LBMA)
}
