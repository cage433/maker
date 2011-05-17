package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

/**
 * Previous width of 20 chars for revalGroup wasn't anywhere near enough. Extending to 255,
 * since snapshot doesn't have many rows even on production - about 5 per day.
 */
class Patch20_WidenSnapshotRevalGroup extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
ALTER TABLE [dbo].[Snapshot] DROP CONSTRAINT [PK_Snapshot_snapshotID]

alter table Snapshot alter column revalGroup varchar(255) not null

ALTER TABLE [dbo].[Snapshot] ADD  CONSTRAINT [PK_Snapshot_snapshotID] PRIMARY KEY CLUSTERED
(
	[snapshotID] ASC,
	[revalGroup] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
""")
  }

  def patchDescription = "Widen the Snapshot.revalGroup column to 255 chars."
}