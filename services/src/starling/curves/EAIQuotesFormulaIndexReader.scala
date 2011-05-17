package starling.curves

import starling.db.DB
import starling.utils.sql.QueryBuilder._
import starling.utils.Log
import starling.market.Index
import starling.market.formula._
import starling.calendar.BusinessCalendars
import collection.MapProxy
import starling.market.rules.Precision
import starling.quantity.{UOM, Conversions, Quantity}
import starling.quantity.UOM._

class EAIQuotesFormulaIndexReader(eai: DB, calendars: BusinessCalendars) extends FormulaIndexes {
  /**
   * map of eaiquoteid to index
   */
  lazy val eaiQuoteMap: Map[Int, FormulaIndex] = loadEAIFormula
    
  def loadEAIFormula = {
    var formulae = Map[Int, FormulaIndex]()
    eai.query((
            select("*, u.name as units")
                    from ("tblquotes q")
                    innerJoin ("tblCurrencies c", ("q.currencyid" eql "c.id"))
                    innerJoin ("tblCalendars cal", ("cal.ID" eql "q.CalendarID"))
                    innerJoin ("tblFCcalendars h", ("h.id" eql "cal.FCCalendarID"))
                    innerJoin ("tblUnits u", ("u.id" eql "q.unitid"))
                    where ("formulaid" isNotNull)
            )) {
      rs => {
        val name = rs.getString("longname")
        val quoteId = rs.getInt("id")
        def getFormula(formulaID: Int): Option[String] = formulaID match {
          case 0 => Some("")
          case _ => {
            eai.queryWithOneResult((
                    select("*")
                            from ("tblformulae")
                            where ("id" eql formulaID)
                    )) {
              rs => {
                val eaiQuoteID = rs.getInt("quoteid")
                val coeff = rs.getDouble("coefficient")
                val nextFormulaID = rs.getString("nextid") match {
                  case null | "" => 0
                  case s => s.toInt
                }
                val op = rs.getString("operator") match {
                  case "+" => "+"
                  case "-" => "-"
                  case "" | null => ""
                  case u => throw new Exception("Uknown op: '" + u + "'")
                }

                getFormula(nextFormulaID).map { f => "" + coeff + "* MKT(" + eaiQuoteID + ")" + op + " " + f }
              }
            }.getOrElse(None)
          }
        }
        val ccy = rs.getUOM("symbol")
        val uom = rs.getUOM("units")
        val default = rs.getInt("defaultPrecision")
        val clearport = rs.getInt("clearportdefaultprecision")
        val prec = Some(Precision(default, clearport))

        val conversion = uom match {
          case MT => {
            // it's not clear what the conversion is for when the index is in MT already
            None
          }
          case _ => Some(Conversions(Map(uom/MT -> rs.getDouble("conversionFactor"))))
        }

        getFormula(rs.getInt("formulaid")).foreach { formula =>
          formulae += (quoteId -> FormulaIndex(name, Formula(formula), ccy, uom, prec, conversion))
        }
      }
    }
    formulae
  }

}