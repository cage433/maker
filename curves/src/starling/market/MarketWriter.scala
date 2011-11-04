package starling.market

import formula.FormulaIndex
import starling.quantity.{UOM, Quantity}
import starling.quantity.UOM._
import starling.quantity.Quantity._
import scala.Any

object MarketWriter {
  val columns = "name	eaiquoteid	type	uom	ccy	businessCalendar	lotSize	tenor	commodity	limSymbol	limMultiplier	defaultPrecision	clearPortPrecision	expiryRule	exchange	volatilityID	indexlevel	forwardMarket	pricingRule	formula	rollBefore	promptness	bblPerMT".split('\t')

  def toCSVLine(line: Map[String, Any]): String = {
    val e = columns.map(c => line.getOrElse(c, "NULL"))
    e.mkString("\t")
  }

  def serialise(market: Either[CommodityMarket, Index]): Map[String, Any] = {
    var map = Map[String, Any]()
    market match {
      case Left(m: FuturesMarket) => {
        map += ("name" -> m.name)
        map += ("eaiquoteid" -> m.eaiQuoteID.get)
        map += ("type" -> m.getClass.getSimpleName)
        map += ("uom" -> m.uom)
        map += ("ccy" -> m.currency)
        map += ("businessCalendar" -> m.businessCalendar.name)
        map += ("lotSize" -> m.lotSize)
        map += ("tenor" -> m.tenor)
        map += ("commodity" -> m.commodity)
        map += ("limSymbol" -> m.limSymbol.map(_.name))
        map += ("limMultiplier" -> m.limSymbol.map(_.multiplier))
        map += ("defaultPrecision" -> m.precision.map(_.default))
        map += ("clearPortPrecision" -> m.precision.map(_.clearPort))
        map += ("expiryRule" -> m.expiryRule.name)
        map += ("exchange" -> m.exchange.name)
        map += ("volatilityID" -> m.volatilityID)
        map += ("bblPerMT" -> m.convert(1.0 (MT), BBL))
      }
      case Right(i) => {
        map += ("name" -> i.name)
        map += ("eaiquoteid" -> i.eaiQuoteID.get)
        map += ("type" -> i.getClass.getSimpleName)
        map += ("uom" -> i.uom)
        map += ("ccy" -> i.priceUOM.numeratorUOM)
        i match {
          case p: PublishedIndex => {
            map += ("businessCalendar" -> p.businessCalendar.name)
            map += ("lotSize" -> p.lotSize)
            map += ("tenor" -> p.tenor.toString)
            map += ("commodity" -> p.commodity.toString)
            map += ("limSymbol" -> p.limSymbol.map(_.name))
            map += ("limMultiplier" ->  p.limSymbol.map(_.multiplier))
          }
          case f: FuturesFrontPeriodIndex => {
            map += ("forwardMarket" -> f.market.name)
            map += ("rollBefore" -> f.rollBeforeDays)
            map += ("promptness" -> f.promptness)
          }
          case s: FormulaIndex => {
            map += ("formula" -> s.formulaString)
          }
          case _ =>
        }
        i match {
          case s: SingleIndex => {
            map += ("indexlevel" -> s.level.name)
          }
          case _ =>
        }

        map += ("defaultPrecision" -> i.precision.map(_.default))
        map += ("clearPortPrecision" -> i.precision.map(_.clearPort))
        map += ("volatilityID" -> None)
        map += ("bblPerMT" -> i.convert(Quantity(1, UOM.MT), UOM.BBL).map(q => q.value))
      }
    }
    map
  }
}