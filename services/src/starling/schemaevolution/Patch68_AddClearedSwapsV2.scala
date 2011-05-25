package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch68_AddClearedSwapsV2 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val tables = List("IntradayTrades", "EAITrade", "GalenaTrade", "SoftmarTrade", "TrinityTrade")
    for (table <- tables) {
      val contraints = """
select O.name name
from sysobjects AS O
left join sysobjects AS T
    on O.parent_obj = T.id
where isnull(objectproperty(O.id,'IsMSShipped'),1) = 0
    and O.name not like '%dtproper%'
    and O.name not like 'dt[_]%'
    and T.name = '""" + table + """'
    and O.name like 'DF__""" + table.subSequence(0, math.min(table.length, 9)) + """__clea%'
    """
      println(contraints)
      val names = starling.queryWithResult(contraints, Map()) {
        rs => rs.getString("name")
      }

      println("dropping contraints for " + names)

      names.map{
        name =>
          writer.update("ALTER TABLE [" + table + "] DROP CONSTRAINT [" + name + "]")
      }

      writer.update("alter table " + table + " drop column [cleared]")
      writer.update("alter table " + table + " add cleared [tinyint] default 0 null")
    }
    writer.update("update EAITrade set cleared = 1, clearinghouse = 'ClearPort' where tradeid like 'C%'")
    writer.update("update IntradayTrades set cleared = 1 where instrument = 'Commodity Swap' and clearinghouse = 'ClearPort'")
  }

  def patchDescription = "Add column for cleared swaps"
}