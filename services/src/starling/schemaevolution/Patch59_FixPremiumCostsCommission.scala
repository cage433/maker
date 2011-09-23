package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.instrument.utils.StarlingXStream
import starling.instrument.{PremiumCosts, Costs}
import starling.services.StarlingInit

class Patch59_FixPremiumCostsCommission extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List("EAITrade", "IntradayTrades", "SoftmarTrade", "RefinedAssignment", "RefinedFixation", "GalenaTrade", "TrinityTrade").foreach {
      table =>
        val sql = "select costs from " + table + " where costs is not null and costs like '%Premium%'"

        val xStream = StarlingXStream.createXStream
        writer.queryForUpdate(sql) {
          rs => {
            val xml = rs.getString("costs")
            try {
              val old: List[Costs] = xStream.fromXML(xml).asInstanceOf[List[Costs]]
              val newCosts = old.map {
                case PremiumCosts(settlementDay, cpty, volume, premium) => new PremiumCosts(settlementDay, cpty, volume, premium)
                case other => other
              }

              val newXML = xStream.toXML(newCosts)
              rs.update(Map("costs" -> newXML))
            } catch {
              case t: Throwable => {
                println("Problem converting: " + xml)
                t.printStackTrace
                throw t
              }
            }
          }
        }

    }
  }

  def patchDescription = "Change commossion costs for premia"
}