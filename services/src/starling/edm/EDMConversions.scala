package starling.edm

import com.trafigura.edm.shared.types.{Quantity => EDMQuantity, CompoundUOM, UnitComponent, FundamentalUOM}
import com.trafigura.edm.marketdata.{MarketDataRow, MarketDataResponse}

import starling.daterange.Day
import starling.quantity.{UOM, Quantity}

import starling.quantity.UOMSymbol._


object EDMConversions {
  implicit def enrichQuantity(q: Quantity) = new {
    def toEDM = toEDMQuantity(q)
  }
  implicit def enrichEDMQuantity(q: EDMQuantity) = new {
    def fromEDM = fromEDMQuantity(q)
  }
  implicit def enrichEDMDate(date: com.trafigura.edm.shared.types.Date) = new {
    def fromEDM = Day.fromLocal(date.datex)
  }
  implicit def enrichMarketDataResponse(response: MarketDataResponse) = new {
    def map[A](f: MarketDataRow => A) = response.rows.map(f)
    def collect[A](f: PartialFunction[List[String], A]) = response.rows.flatMap(row => f.lift(row.data))
  }

  def fromEDMQuantity(q : EDMQuantity) : Quantity = {
    val amount = q.amount.get  // No idea why this is optional in EDM
    val uom = UOM.fromSymbolMap(q.uom.components.map {
      case uc => edmToStarlingUomSymbol(uc.fundamental) -> uc.exponent
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
        fundamental = starlingToEdmUom.getOrElse(starlingUOMSymbol, FundamentalUOM(starlingUOMSymbol.toString))
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
    TONNE_SYMBOL -> "MTS",
    POUND_SYMBOL -> "LBS"
  ).mapValues(FundamentalUOM(_))

  val edmToStarlingUomSymbol = starlingToEdmUom.map(_.swap)
}
