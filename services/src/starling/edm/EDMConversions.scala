package starling.edm

import starling.quantity.UOMSymbol._
import com.trafigura.edm.shared.types.{Quantity => EDMQuantity, CompoundUOM, UnitComponent, FundamentalUOM}
import starling.quantity.{UOM, Quantity}


/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 14/06/11
 * Time: 16:16
 * To change this template use File | Settings | File Templates.
 */

object EDMConversions {

  def fromEDMQuantity(q : EDMQuantity) : Quantity = {
    val amount = q.amount.get  // No idea why this is optional in EDM
    val uom = UOM.fromSymbolMap(q.uom.components.map {
      case uc => edmToStarlingUomSymbol(uc.fundamental.name) -> uc.exponent
    }.toMap)
    Quantity(amount, uom)

  }

  def toEDMQuantity(q : Quantity) : EDMQuantity = {
    val symbolPowers = q.uom.asSymbolMap()

    // create edm UOMs, EDM symbol list is GBP, USD, JPY, RMB, MTS, LBS

    val unitComponents = symbolPowers.map{
      case (starlingUOMSymbol, power) => UnitComponent(
        oid = 0,
        exponent = power,
        fundamental = FundamentalUOM(starlingToEdmUom.getOrElse(starlingUOMSymbol, starlingUOMSymbol.toString))
       )
    }.toList

    EDMQuantity(Some(q.value), CompoundUOM(unitComponents))
  }

  // EDM UOM to Starling UOM
  val starlingToEdmUom = Map(
    gbp -> "GBP",
    usd -> "USD",
    jpy -> "JPY",
    cny -> "RMB",
    eur -> "EUR",
    TONNE_SYMBOL -> "MTS",
    POUND_SYMBOL -> "LBS"
  )

  val edmToStarlingUomSymbol = starlingToEdmUom.map(_.swap)
}
