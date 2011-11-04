package starling.curves

import starling.utils.Log
import starling.props.PropsHelper
import starling.services.StarlingInit
import java.lang.reflect.Method
import starling.market.{MarketWriter, CommodityMarket, PublishedIndex}

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
        val eaiExtras = eaiIndexes.map(_.eaiQuoteID) -- starlingIndexes.map(_.eaiQuoteID)
        if (eaiExtras.size > 0) {
          Log.warn("EAI has some extra indexes: " + eaiExtras.size + ", " + eaiExtras)
          val map = eaiIndexes.map(i => i.eaiQuoteID -> i).toMap
          eaiExtras.map{
            quoteID => {
              val index = map(quoteID)
              val s = MarketWriter.serialise(Right(index))
              println(MarketWriter.toCSVLine(s))
            }
          }
        }
        val starlingIndexesMap = starlingIndexes.map(i => i.eaiQuoteID -> i).toMap

        (eaiIndexes.filterNot(i => eaiExtras.contains(i.eaiQuoteID))).map {
          eaiIndex => {
            val starlingIndex = starlingIndexesMap(eaiIndex.eaiQuoteID)

            (eaiIndex, starlingIndex) match {
              case (e: PublishedIndex, s: PublishedIndex) => {
                val fields = classOf[PublishedIndex].getDeclaredMethods.filter(isFieldAccessor)
                fields.map {
                  case field => {
                    val a = field.invoke(e)
                    val b = field.invoke(s)
                    if(a != b) {
                      Log.warn(e + " is not the same as " + s + " on " + field.getName + ": " + a + " != " + b)
                    }
                  }
                }
              }
              case _ =>
            }
          }
        }
      }

      println("")
      println("")
      println("")
      println("")

      {
        val eaiMarkets: Set[CommodityMarket] = eai.allMarkets.toSet.asInstanceOf[Set[CommodityMarket]]
        val starlingMarkets: Set[CommodityMarket] = starling.allMarkets.toSet.asInstanceOf[Set[CommodityMarket]]
        val eaiExtras = eaiMarkets.map(_.eaiQuoteID) -- starlingMarkets.map(_.eaiQuoteID)
        if (eaiExtras.size > 0) {
          Log.warn("EAI has some extra markets: " + eaiExtras)
          val map = eaiMarkets.map(i => i.eaiQuoteID -> i).toMap
          eaiExtras.map{
            quoteID => {
              val market = map(quoteID)
              val s = MarketWriter.serialise(Left(market))
              println(MarketWriter.toCSVLine(s))
            }
          }
        }
        val starlingMarketsMap = starlingMarkets.map(i => i.eaiQuoteID -> i).toMap

        (eaiMarkets.filterNot(i => eaiExtras.contains(i.eaiQuoteID))).map {
          eaiMarket => {
            val starlingMarket = starlingMarketsMap(eaiMarket.eaiQuoteID)
            (eaiMarket, starlingMarket) match {
              case (e: CommodityMarket, s: CommodityMarket) => {
                val fields = classOf[CommodityMarket].getDeclaredMethods.filter(isFieldAccessor)
                fields.map {
                  case field => {
                    try {
                      val a = field.invoke(e)
                      val b = field.invoke(s)
                      if (a != b) {
                        Log.warn(e + " is not the same as " + s + " on " + field.getName + ": " + a + " != " + b)
                      }
                    }
                    catch {
                      case e =>
                    }
                  }
                }
              }
              case _ =>
            }
          }
        }
      }
    }
  }

  private def isFieldAccessor(method: Method) = {
    val bla =
      method.getName.contains("copy") || method.getName.contains("apply") ||
      method.getName.contains("curried") || method.getName.contains("curry") ||
      method.getName.contains("tupled") || method.getName.toLowerCase.contains("cache") ||
      method.getName.contains("product") ||method.getName.contains("hashCode")
    !bla && method.getParameterTypes.isEmpty
  }

  def main(args: Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    val init = StarlingInit.runningDevInstance
    init.start
    val eai = new EAIMarketLookup(init.eaiSqlServerDB, init.expiryRules)
    val starling = new StarlingMarketLookup(init.starlingDB, init.businessCalendars, init.expiryRules)
    compare(eai, starling)
    
    init.stop
  }
}