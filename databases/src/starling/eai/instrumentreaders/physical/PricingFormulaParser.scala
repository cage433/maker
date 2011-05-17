package starling.eai.instrumentreaders.physical

import starling.market.formula.Formula
import xml.XML
import starling.market.SingleIndex

object PricingFormulaParser {
  def parse(xmlStr: String, marketLookup: (Int) => SingleIndex): Formula = {
    val xml = XML.loadString(xmlStr)
    val formula = new StringBuilder
    xml \ "token" map {
      token => (token \ "typeid").text.toInt match {
        case 1 => {
          // Constant
          val e = (token \ "op").text + (token \ "p1").text.toDouble
          formula ++= e
        }
        case 2 => {
          // Quote -- e.g. 0.2 * QuoteID
          val op = (token \ "op").text
          if (op.nonEmpty) {
            formula ++= (op)
          }
          val co = token \ "co"
          if (co.nonEmpty) {
            val coeff = co.text.toDouble
            formula ++= (coeff + " * ")
          }
          val eaiQuoteID = (token \ "p1id").text.toInt
          val index = marketLookup(eaiQuoteID) // will throw an exception if we don't know this market
          formula ++= ("MKT(" + eaiQuoteID + ")")
        }
        case 3 => {
          // Diff -- e.g. DIFF("Sulphur", .2)
          // this is just a hack used because of some Parcels problem in Aspect. It is telling Aspect
          // that there is an adjustment to the formula because of Sulphur. If the parcels were included as they
          // should the sulphur component would apparently be in the parcel.
          // p1 is just an identifier, not a market. So "sulphur" or "fortis" etc.
          val e = (token \ "op").text + (token \ "p2").text.toDouble
          formula ++= e
        }
      }
    }
    val formulaStr = formula.toString
    Formula(formulaStr)
  }
}
