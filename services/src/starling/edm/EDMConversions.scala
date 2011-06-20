package starling.edm

import starling.quantity.UOMSymbol._
import com.trafigura.edm.shared.types.{Quantity => EDMQuantity, CompoundUOM, UnitComponent, FundamentalUOM}
import starling.quantity.{UOMSymbol, Quantity}


/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 14/06/11
 * Time: 16:16
 * To change this template use File | Settings | File Templates.
 */

object EDMConversions {

  def toEDMQuantity(q : Quantity) : EDMQuantity = {
    val symbolPowers = q.uom.asSymbolMap()

    // create edm UOMs, EDM symbol list is GBP, USD, JPY, RMB, MTS, LBS

    val unitComponents = symbolPowers.map{
      case (starlingUOMSymbol, power) => UnitComponent(
        oid = 0,
        exponent = power,
        fundamental = FundamentalUOM(starlingToEdmUomMap(starlingUOMSymbol))
       )
    }.toList

    EDMQuantity(Some(q.value), CompoundUOM(unitComponents))
  }

  // EDM UOM to Starling UOM
    val edmToStarlingUomMap = Map[String, UOMSymbol](
      "GBP" -> gbp,
      "USD" -> usd,
      "JPY" -> jpy,
      "RMB" -> cny,
      "MTS" -> TONNE_SYMBOL,
      "LBS" -> POUND_SYMBOL)

    // Starling to EDM UOM
    val starlingToEdmUomMap : Map[UOMSymbol, String] = edmToStarlingUomMap.map(_.swap)
}
