package starling.edm

import com.trafigura.tradecapture.internal.refinedmetal.{Metal, Market => RefMarket}
import com.trafigura.tradecapture.internal.refinedmetal.Market._
import starling.market._
import java.lang.Exception

/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 22/06/11
 * Time: 14:03
 * To change this template use File | Settings | File Templates.
 */

object RefinedTacticalRefDataConversions {

  import Market._

  val commodityNameToLMEMarket = Map(
    "Nickel" -> LME_NICKEL,
    "Copper" -> LME_COPPER,
    "Steel" -> LME_STEEL_BILLETS,
    "NASAAC" -> LME_NASAAC,
    "Tin" -> LME_TIN,
    "Aluminium Alloy" -> LME_ALUMINIUM_ALLOY,
    "Primary Aluminium" -> LME_ALUMINIUM,
    "Zinc" -> LME_ZINC,
    "Lead" -> LME_LEAD
  )

  val commodityNameToSHFEMarket = Map(
    "Zinc" -> SHANGHAI_ZINC,
    "Steel" -> STEEL_REBAR_SHANGHAI,
    "Copper" -> SHANGHAI_COPPER,
    "Primary Aluminium" -> SHANGHAI_ALUMINUIUM
  )

  val metalNameToCMXMarket = Map(
    "Copper" -> COMEX_HIGH_GRADE_COPPER
  )

  val metalNameToWuxiMarket = Map(
    "Nickel" -> EXBG_NICKEL
  )

  def guessAtIndex(exchange : RefMarket, metal : Metal) : SingleIndex = {
    val metalToMarketMap : Map[String, FuturesMarket] = exchange.mappingCode match {
      case `LME` => commodityNameToLMEMarket
      case `SHFE` => commodityNameToSHFEMarket
      case `CMX` => metalNameToCMXMarket
      case `WUXI` => metalNameToWuxiMarket
      case other => throw new Exception("Unrecognised exchange " + other + ", metal was " + metal.name)
    }

    if (! metalToMarketMap.contains(metal.name))
      throw new Exception("No market found for " + exchange.mappingCode + ", " + metal.name)
    val futuresMarket = metalToMarketMap(metal.name)

    exchange.mappingCode match {
      case `LME` => LmeCashSettlementIndex(futuresMarket)
      case `SHFE` | `WUXI` => FuturesFrontPeriodIndex(futuresMarket, Level.Settle)
      case _ => FuturesFrontPeriodIndex(futuresMarket)
    }

  }

}