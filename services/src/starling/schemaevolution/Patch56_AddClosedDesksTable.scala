package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch56_AddClosedDesksTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("drop table EAIClosedBooks")

    writer.update("""
CREATE TABLE [dbo].[ClosedDesks](
  [id] [int] IDENTITY(1,1) NOT NULL,
  [desk] varchar(255) NOT NULL,
  [closed] datetime NOT NULL,
  [error] [text] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
  """
      )
  }

  def patchDescription = "add closed desks table"
}