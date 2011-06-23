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

case class InvalidUomException(msg : String) extends Exception(msg)

object EDMConversions {

  implicit def fromEDMQuantity(q : EDMQuantity) : Quantity = {
    val amount = q.amount match {
      case Some(amt) => amt
      case None => throw new Exception("Invalid quantity - no amount")
    }  // No idea why this is optional in EDM
    val uom = UOM.fromSymbolMap(q.uom.components.map {
      case uc => {
        edmToStarlingUomSymbol.get(uc.fundamental.name) match {
          case Some(uomSymbol) => uomSymbol -> uc.exponent
          case None => throw new InvalidUomException(uc.fundamental.name)
        }
      }
    }.toMap)
    Quantity(amount, uom)

  }

  implicit def toEDMQuantity(q : Quantity) : EDMQuantity = {
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
