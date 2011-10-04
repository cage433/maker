package starling.market

import formula.FormulaIndex
import starling.quantity.{UOM, Quantity}

object MarketWriter {
  val columns = "name	eaiquoteid	type	uom	ccy	businessCalendar	lotSize	tenor	commodity	limSymbol	limMultiplier	defaultPrecision	clearPortPrecision	expiryRule	exchange	volatilityID	indexlevel	forwardMarket	pricingRule	formula	rollBefore	promptness	bblPerMT".split('\t')

  def serialise(market: Either[CommodityMarket, Index]): Map[String, Any] = {
    var map = Map[String, Any]()
    market match {
      case Left(m) => {
        throw new Exception("Not implemented for market")
        map += ("name" -> m.name)
        map += ("eaiquoteid" -> m.eaiQuoteID)
        map += ("type" -> m.getClass.getSimpleName)
        map += ("uom" -> m.uom)
        map += ("ccy" -> m.currency)
        map += ("businessCalendar" -> m.businessCalendar.name)
        map += ("lotSize" -> m.lotSize)
      }
      case Right(i) => {
        map += ("name" -> i.name)
        map += ("eaiquoteid" -> i.eaiQuoteID)
        map += ("type" -> i.getClass.getSimpleName)
        map += ("uom" -> i.uom)
        map += ("ccy" -> i.priceUOM.numeratorUOM)
        i match {
          case p: PublishedIndex => {
            map += ("businessCalendar" -> p.businessCalendar.name)
            map += ("lotSize" -> p.lotSize)
            map += ("tenor" -> p.tenor.toString)
            map += ("commodity" -> p.commodity.toString)
            p.limSymbol.map(l => map += ("limSymbol" -> l.name))
            p.limSymbol.map(l => map += ("limMultiplier" -> l.multiplier))
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

        i.precision.map(p => map += ("defaultPrecision" -> p.default))
        i.precision.map(p => map += ("clearPortPrecision" -> p.clearPort))
        map += ("volatilityID" -> None)
        map += ("bblPerMT" -> i.convert(Quantity(1, UOM.MT), UOM.BBL).map(q => q.value))
      }
    }
    map
  }
}