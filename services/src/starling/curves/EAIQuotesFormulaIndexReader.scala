package starling.curves

import starling.db.DB
import starling.utils.sql.QueryBuilder._
import starling.utils.Log
import starling.market.formula._
import starling.calendar.BusinessCalendars
import collection.MapProxy
import starling.market.rules.Precision
import starling.quantity.{UOM, Conversions, Quantity}
import starling.quantity.UOM._
import starling.market.{Commodity, OilCommodity, Index}

class EAIQuotesFormulaIndexReader(eai: DB, calendars: BusinessCalendars) extends FormulaIndexes {
  /**
   * map of eaiquoteid to index
   */
  lazy val eaiQuoteMap: Map[Int, FormulaIndex] = loadEAIFormula
    
  def loadEAIFormula = {
    var formulae = Map[Int, FormulaIndex]()
    eai.query((
            select("*, u.name as units, pc.name as productcategory")
                    from ("tblquotes q")
                    innerJoin ("tblCurrencies c", ("q.currencyid" eql "c.id"))
                    innerJoin ("tblCalendars cal", ("cal.ID" eql "q.CalendarID"))
                    innerJoin ("tblFCcalendars h", ("h.id" eql "cal.FCCalendarID"))
                    innerJoin ("tblProductCategories pc", ("pc.id" eql "q.ProductCategoryID"))
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

        val conversion = if(oilCommodities.contains(rs.getString("productcategory"))) {
          // tblquotes has a bbl/mt conversion for oil commodities
          Some(Conversions(Map(BBL/MT -> rs.getDouble("conversionFactor"))))
        } else {
          None
        }

        getFormula(rs.getInt("formulaid")).foreach { formula =>
          formulae += (quoteId -> FormulaIndex(name, Formula(formula), ccy, uom, prec, conversion))
        }
      }
    }
    formulae
  }

  // this is a hack that will get nicer when alex's market reading code is done
  val oilCommodities = List(
    "Crude",
    "Freight",
    "Fuel Oil",
    "Gas Oil",
    "Gasoline",
    "Jet / Kero",
    "Metal",
    "Naphtha",
    "Vegetable Oil")
}