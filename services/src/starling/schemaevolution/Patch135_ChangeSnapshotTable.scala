package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchContext, Patch}
import starling.daterange.Timestamp
import starling.gui.api.{MarketDataSelection, PricingGroup}
import starling.utils.sql.PersistAsBlob

class Patch135_ChangeSnapshotTable extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("drop table MarketDataTag")
    writer.update("""
    create table dbo.MarketDataSnapshots (
      snapshotID int identity(1,1) not null,
      snapshotTime datetime NOT NULL,
	    marketDataSelection varchar(2048) NULL,
	    snapshotType varchar(32),
	    commitId int NULL
    ) ON [PRIMARY]
""")
    writer.insert("MarketDataSnapshots",
      Map(
        "snapshotTime" -> new Timestamp,
        "marketDataSelection" -> PersistAsBlob(MarketDataSelection(Some(PricingGroup.Metals))),
        "snapshotType" -> "Valuation",
        "commitId" -> 0
      )
    )
  }
}