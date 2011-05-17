package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch57_ChangeClosedDesksTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("truncate table EAITrade")
    writer.update("truncate table EAITradeUTP")

    writer.update("drop table ClosedDesks")

    writer.update("""
CREATE TABLE [dbo].[ClosedDesks](
  [id] [int] IDENTITY(1,1) NOT NULL,
  [desk] varchar(255) NOT NULL,
  [tradeTimestamp] datetime NOT NULL,
  [closedDay] datetime NOT NULL,
  [error] [text] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
  """
      )
  }

  def patchDescription = "change closed desks table"
}