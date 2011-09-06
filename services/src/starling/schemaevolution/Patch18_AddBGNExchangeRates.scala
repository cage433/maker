package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.daterange.Day
import starling.utils.sql.QueryBuilder._
import starling.utils.ImplicitConversions._
import org.testng.annotations.Test
import starling.marketdata.SpotFXData
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.instrument.utils.StarlingXStream
import collection.immutable.Map
import starling.services.StarlingInit


object AddBGNExchangeRates{
  val revalGroups = List("Freight", "Oil", "GMA", "Metals")

  def snapshots(starling: RichDB, revalGroup: String): List[(Int, Day)] = {
    val query = select("*") from "Snapshot" where ("revalGroup" eql revalGroup)
    var lst = List[(Int, Day)]()
    starling.query(query) {
      rs =>
        val observationDay = rs.getDay("observationDay")
        val snapshotID = rs.getInt("snapshotID")
        lst = (snapshotID, observationDay) :: lst
    }
    lst
  }

  def apply(starling: RichDB, writer : DBWriter, rates: Map[Day, Double]) {
    revalGroups.foreach {
      grp =>
        snapshots(starling, grp).foreach {
          case (snapshotID, observationDay) =>
            if (rates.contains(observationDay)) {
              val data = new SpotFXData(Quantity(rates(observationDay), USD / BGN))
              val dataXml = StarlingXStream.write(data.marshall)
              writer.insert(
                "SnapshotData",
                Map(
                  "snapshotID" -> snapshotID,
                  "dataTypeKey" -> "spotFX",
                  "subTypeKey" -> "BGN",
                  "data" -> dataXml
                  ))
            }
        }
    }
  }

}
class Patch18_AddBGNExchangeRates extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter): Unit = {
    AddBGNExchangeRates(starling, writer, Patch18_AddBGNExchangeRates.historicRates)
  }

  def patchDescription = "Add historic USD/BGN rates"
}

object Patch18_AddBGNExchangeRates {
  val historicRates = Map(
    Day(2010, 2, 17) -> 1.43633,
    Day(2010, 2, 18) -> 1.44933,
    Day(2010, 2, 19) -> 1.43661,
    Day(2010, 2, 20) -> 1.44500,
    Day(2010, 2, 21) -> 1.43683,
    Day(2010, 2, 22) -> 1.43790,
    Day(2010, 2, 23) -> 1.44744,
    Day(2010, 2, 24) -> 1.44465,
    Day(2010, 2, 25) -> 1.44439,
    Day(2010, 2, 26) -> 1.43545,
    Day(2010, 2, 27) -> 1.43188,
    Day(2010, 2, 28) -> 1.43613,
    Day(2010, 3, 1) -> 1.44172,
    Day(2010, 3, 2) -> 1.43706,
    Day(2010, 3, 3) -> 1.42818,
    Day(2010, 3, 4) -> 1.44044,
    Day(2010, 3, 5) -> 1.43603,
    Day(2010, 3, 6) -> 1.43746,
    Day(2010, 3, 7) -> 1.43401,
    Day(2010, 3, 8) -> 1.43515,
    Day(2010, 3, 9) -> 1.43802,
    Day(2010, 3, 10) -> 1.43234,
    Day(2010, 3, 11) -> 1.42998,
    Day(2010, 3, 12) -> 1.42066,
    Day(2010, 3, 13) -> 1.42066,
    Day(2010, 3, 14) -> 1.42084,
    Day(2010, 3, 15) -> 1.43017,
    Day(2010, 3, 16) -> 1.42001,
    Day(2010, 3, 17) -> 1.42395,
    Day(2010, 3, 18) -> 1.43737,
    Day(2010, 3, 19) -> 1.44523,
    Day(2010, 3, 20) -> 1.44523,
    Day(2010, 3, 21) -> 1.44550,
    Day(2010, 3, 22) -> 1.44171,
    Day(2010, 3, 23) -> 1.44925,
    Day(2010, 3, 24) -> 1.46786,
    Day(2010, 3, 25) -> 1.47205,
    Day(2010, 3, 26) -> 1.45857,
    Day(2010, 3, 27) -> 1.46053,
    Day(2010, 3, 28) -> 1.45053,
    Day(2010, 3, 29) -> 1.45157,
    Day(2010, 3, 30) -> 1.45715,
    Day(2010, 3, 31) -> 1.44803,
    Day(2010, 4, 1) -> 1.43975,
    Day(2010, 4, 2) -> 1.44867,
    Day(2010, 4, 3) -> 1.44867,
    Day(2010, 4, 4) -> 1.44943,
    Day(2010, 4, 5) -> 1.45065,
    Day(2010, 4, 6) -> 1.46017,
    Day(2010, 4, 7) -> 1.46617,
    Day(2010, 4, 8) -> 1.46385,
    Day(2010, 4, 9) -> 1.44842,
    Day(2010, 4, 10) -> 1.44842,
    Day(2010, 4, 11) -> 1.43486,
    Day(2010, 4, 12) -> 1.43955,
    Day(2010, 4, 13) -> 1.43630,
    Day(2010, 4, 14) -> 1.43273,
    Day(2010, 4, 15) -> 1.44014,
    Day(2010, 4, 16) -> 1.44845,
    Day(2010, 4, 17) -> 1.45028,
    Day(2010, 4, 18) -> 1.45106,
    Day(2010, 4, 19) -> 1.44975,
    Day(2010, 4, 20) -> 1.45503,
    Day(2010, 4, 21) -> 1.46111,
    Day(2010, 4, 22) -> 1.47206,
    Day(2010, 4, 23) -> 1.46141,
    Day(2010, 4, 24) -> 1.46141,
    Day(2010, 4, 25) -> 1.46543,
    Day(2010, 4, 26) -> 1.46042,
    Day(2010, 4, 27) -> 1.48631,
    Day(2010, 4, 28) -> 1.48117,
    Day(2010, 4, 29) -> 1.47676,
    Day(2010, 4, 30) -> 1.47134,
    Day(2010, 5, 1) -> 1.47046,
    Day(2010, 5, 2) -> 1.46714,
    Day(2010, 5, 3) -> 1.48276,
    Day(2010, 5, 4) -> 1.50790,
    Day(2010, 5, 5) -> 1.52559,
    Day(2010, 5, 6) -> 1.54863,
    Day(2010, 5, 7) -> 1.53257,
    Day(2010, 5, 8) -> 1.53726,
    Day(2010, 5, 9) -> 1.51125,
    Day(2010, 5, 10) -> 1.52955,
    Day(2010, 5, 11) -> 1.54816,
    Day(2010, 5, 12) -> 1.54784,
    Day(2010, 5, 13) -> 1.56174,
    Day(2010, 5, 14) -> 1.58232,
    Day(2010, 5, 15) -> 1.58232,
    Day(2010, 5, 16) -> 1.58353,
    Day(2010, 5, 17) -> 1.57655,
    Day(2010, 5, 18) -> 1.60666,
    Day(2010, 5, 19) -> 1.57436,
    Day(2010, 5, 20) -> 1.56854,
    Day(2010, 5, 21) -> 1.55447,
    Day(2010, 5, 22) -> 1.55447,
    Day(2010, 5, 23) -> 1.55918,
    Day(2010, 5, 24) -> 1.58458,
    Day(2010, 5, 25) -> 1.58086,
    Day(2010, 5, 26) -> 1.60931,
    Day(2010, 5, 27) -> 1.58283,
    Day(2010, 5, 28) -> 1.59425,
    Day(2010, 5, 29) -> 1.58175,
    Day(2010, 5, 30) -> 1.59193,
    Day(2010, 5, 31) -> 1.58905,
    Day(2010, 6, 1) -> 1.60211,
    Day(2010, 6, 2) -> 1.59714,
    Day(2010, 6, 3) -> 1.60804
    )
}