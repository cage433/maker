package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.instrument.TradeableType


class Patch150_AddObservationDayToSnapshot extends Patch{

  def populateObservationDayField(starlingDB: RichDB, writer : DBWriter) {
    starlingDB.query("select * from marketdatasnapshots where SnapshotType = 'Valuation'", Map()) {
      row => {

        val id = row.getInt("snapshotID")
        val observationDay = row.getTimestamp("snapshotTime").day
        import starling.dbx.QueryBuilder._
        writer.update("marketdatasnapshots", Map("observationDay" -> observationDay), ("snapshotID" eql id))

      }
    }

  }

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter table marketdatasnapshots add observationDay datetime null")
    populateObservationDayField(starling, writer)
  }

}