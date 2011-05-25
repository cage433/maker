package starling.schemaevolution


import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit

class Patch49_RemoveInterestRateProducts extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {


    List("EAITrade", "GalenaTrade", "TrinityTrade").foreach {
      table => {
        val run = starling.queryWithOneResult("select top 1 * from " + table) {
          rs => rs.hasColumn("fixedBasis")
        } match {
          case Some(true) => {
            writer.update(
              """
                delete from %s where instrument in ('Euro Dollar Future', 'Interest Rate Option', 'Interest Rate Swap')
              """ % table
              )

            List("fixedBasis", "floatBasis", "fixedPaymentFreq", "floatPaymentFreq").foreach {
              column =>
                writer.update(
                  """
                    alter table %s drop column %s
                  """ % (table, column)
                  )
            }
          }
          case _ =>
        }

      }
    }
  }

  def patchDescription = "Remove interest rate products from trade stores"
}
