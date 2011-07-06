package starling.eai.instrumentreaders.physical

import starling.systemofrecord.InstrumentReader
import starling.daterange.DateRange
import starling.instrument.CommoditySwap
import starling.richdb.RichResultSetRow
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.eai.instrumentreaders.EAISystemOfRecord
import starling.market.formula.FormulaIndex
import starling.instrument.physical.{Incoterm, Cargo}
import starling.market.Index
import starling.pricingschedule.PricingScheduleXMLParser
import starling.market.rules.{Precision, CommonPricingRule}

class CargoReader extends InstrumentReader {

  import EAISystemOfRecord._

  def canHandle(rs: RichResultSetRow) = rs.getInt("TradeType") == CARGO

  override def create(rs: RichResultSetRow) = {
    val pricingXML = rs.isNull("PricingFormulaXML") match {
      case true => {
        val xml = rs.getString("PricingFormulaDefaultXML")
        assert(xml == rs.getString("PricingFormulaPlutoXML"), xml + " != " + rs.getString("PricingFormulaPlutoXML"))
        xml
      }
      case false => rs.getString("PricingFormulaXML")
    }
    val formula = PricingFormulaParser.parse(pricingXML, Index.singleIndexFromEAIQuoteID _)
    val uom = rs.getUOM("priceuom")

    val roundingID = rs.getInt("roundingID")
    val precision = Some(Precision(roundingID, roundingID))

    val index = new FormulaIndex(formula.toString, formula, USD, uom, precision, None, None)
    index.verify
    val blDate = rs.getDay("plutoBLDate")
    val quantity = rs.getQuantity("Quantity")

    val scheduleXML = rs.isNull("UnderlyingScheduleXML") match {
      case true => {
        val xml = rs.getString("UnderlyingScheduleDefaultXML")
        assert(xml == rs.getString("UnderlyingSchedulePlutoXML"), xml + " != " + rs.getString("UnderlyingScheduleDefaultXML"))
        xml
      }
      case false => rs.getString("UnderlyingScheduleXML")
    }
    val schedule = try {
      PricingScheduleXMLParser.parse(scheduleXML, Some(blDate))
    }
    catch {
      case e => {
        e.printStackTrace
        throw e
      }
    }

    val swapPricingRule = rs.getSwapPricingRule("PricingRule")

    val incoterm = rs.getString("incoterm") match {
      case Incoterm(i) => i
    }

    Cargo(quantity, incoterm, blDate, index, schedule, swapPricingRule)
  }
}
