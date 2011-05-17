package starling.schemaevolution

import system.Patch
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit


class Patch23_WidenSnapshotDataSubTypeKey extends Patch{
   protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""
        ALTER TABLE [dbo].[SnapshotData] DROP CONSTRAINT [PK_SnapshotData_snapshotID]

        alter table [SnapshotData]
        alter column [subtypekey] varchar(255) not null

        ALTER TABLE [dbo].[SnapshotData] ADD  CONSTRAINT [PK_SnapshotData_snapshotID] PRIMARY KEY CLUSTERED
        (
          [snapshotID] ASC,
          [dataTypeKey] ASC,
          [subTypeKey] ASC
        )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
      """)
   }
  def patchDescription = "Widen the SnapshotData.subTypeKey column to 255 chars."

}