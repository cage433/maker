package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit

class Patch40_RemoveLongExpiredTrades extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List( "EAITrade", "GalenaTrade", "RefinedAssignment", "RefinedFixation", "SoftmarTrade", "TrinityTrade" ).foreach{
      table =>
        val idsToDelete = """
          (select id from %s t
               inner join (
                  select tradeID, max(expiryDay_cache) as maxExpiryDay from %s
                  group by tradeID
                 ) t2
              on t.tradeID = t2.tradeID
              where t2.maxExpiryDay < '2009-10-01')""" % (table, table)

        starling.query(
          "select count(*) as count from %s where id in %s " % (table, idsToDelete)){
          rs => println("About to delete " + rs.getInt("count") + " rows")
        }

        writer.update(" delete from %s where id in %s" % (table, idsToDelete))
    }

  }

  def patchDescription = "Remove trades whose versions have a maximum expiry day cache befoe 01 Oct 2009"
}