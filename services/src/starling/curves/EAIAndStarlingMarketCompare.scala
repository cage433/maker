package starling.curves

import starling.utils.Log
import starling.market.PublishedIndex

object EAIAndStarlingMarketCompare {
  /**
   * Compares markets from both sources and prints warnings about the differences
   *
   * TODO only compares published indexes in detail at the moment
   */
  def compare(eai: EAIMarketLookup, starling: StarlingMarketLookup) {
    Log.infoWithTime("Comparing EAI and Starling markets") {
      {
        val eaiIndexes = eai.allIndexes.toSet
        val starlingIndexes = starling.allIndexes.toSet
        val eaiExtras = eaiIndexes.filter(i => !starlingIndexes.find(_.name == i.name).isDefined)
        if (eaiExtras.size > 0) {
          Log.warn("EAI has some extra indexes: " + eaiExtras)
        }
        val starlingIndexesMap = starlingIndexes.map(i => i.name -> i).toMap

        (eaiIndexes -- eaiExtras).map {
          eaiIndex => {
            val starlingIndex = starlingIndexesMap(eaiIndex.name)

            (eaiIndex, starlingIndex) match {
              case (e: PublishedIndex, s: PublishedIndex) => {
                val error = e + " is not the same as " + s + " for "
                if (e.lotSize != s.lotSize) Log.warn(error + "lotSize")
                if (e.uom != s.uom) Log.warn(error + "uom")
                if (e.currency != s.currency) Log.warn(error + "currency")
                if (e.businessCalendar != s.businessCalendar) Log.warn(error + "businessCalendar")
                if (e.commodity != s.commodity) Log.warn(error + "commodity")
                if (e.conversions != s.conversions) Log.warn(error + "conversions")
                if (e.limSymbol != s.limSymbol) {Log.warn(error + "limSymbol")}
                if (e.precision != s.precision) Log.warn(error + "precision")
                if (e.level != s.level) Log.warn(error + "level")
              }
              case _ =>
            }
            if(eaiIndex != starlingIndex) {
              println("different: " + (eaiIndex, starlingIndex))
            }
          }
        }
      }
      {
        val eaiMarkets = eai.allMarkets.toSet
        val starlingMarkets = starling.allMarkets.toSet
        val eaiExtras = eaiMarkets.filter(i => !starlingMarkets.find(_.name == i.name).isDefined)
        if (eaiExtras.size > 0) {
          Log.warn("EAI has some extra markets: " + eaiExtras)
        }
        val starlingMarketsMap = starlingMarkets.map(i => i.name -> i).toMap

        (eaiMarkets -- eaiExtras).map {
          eaiMarket => {
            val starlingMarket = starlingMarketsMap(eaiMarket.name)
            if(eaiMarket != starlingMarket) {
              println("different: " + (eaiMarket, starlingMarket))
            }
          }
        }
      }
    }
  }
}