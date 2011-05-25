package starling.schemaevolution

import system.Patch
import starling.daterange.Day
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit

class Patch27_AddEvenMoreBGNRates extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    AddBGNExchangeRates(starling, writer, Patch27_AddEvenMoreBGNRates.historicRates)
  }

  def patchDescription = "Add USD/BGN rates from 22nd June to 11th July"
}

object Patch27_AddEvenMoreBGNRates {
  val historicRates = Map(
    Day(2010, 6, 22) -> .62730,
    Day(2010, 6, 23) -> .62947,
    Day(2010, 6, 24) -> .63034,
    Day(2010, 6, 25) -> .63278,
    Day(2010, 6, 26) -> .63278,
    Day(2010, 6, 27) -> .63292,
    Day(2010, 6, 28) -> .62793,
    Day(2010, 6, 29) -> .62319,
    Day(2010, 6, 30) -> .62570,
    Day(2010, 7, 1) -> .64007,
    Day(2010, 7, 2) -> .64255,
    Day(2010, 7, 3) -> .64311,
    Day(2010, 7, 4) -> .64200,
    Day(2010, 7, 5) -> .64113,
    Day(2010, 7, 6) -> .64546,
    Day(2010, 7, 7) -> .64582,
    Day(2010, 7, 8) -> .64904,
    Day(2010, 7, 9) -> .64634,
    Day(2010, 7, 10) -> .64634,
    Day(2010, 7, 11) -> .64649
    )

}