package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch54_AddEAIBooksTable extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
CREATE TABLE [dbo].[EAIClosedBooks](
  [id] [int] IDENTITY(1,1) NOT NULL,
  [bookID] [int] NOT NULL,
  [closed] datetime NOT NULL,
  [error] [text] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
  """
      )
  }

  def patchDescription = "add EAIClosedBooks table"
}