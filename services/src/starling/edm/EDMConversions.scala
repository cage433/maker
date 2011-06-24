package starling.edm

import com.trafigura.edm.shared.types.{Quantity => EDMQuantity, Currency => ECurrency, Percentage => EPercentage,
                                       CompoundUOM, UnitComponent, FundamentalUOM}

import starling.quantity.UOMSymbol._
import starling.daterange.{Tenor, SimpleDateRange, DateRange, Day}

import starling.utils.ImplicitConversions._
import starling.quantity.{UOMSymbol, Percentage, UOM, Quantity}
import starling.utils.StarlingEnum
import com.trafigura.marketdataservice.{MaturityType, Maturity, NamedMaturity, RelativeMaturity}

object EDMConversions {
  implicit def enrichQuantity(q: Quantity) = new {
    def toEDM = toEDMQuantity(q)
  }
  implicit def enrichTenor(tenor: Tenor) = new {
    def toEDM: Maturity = (tenor, tenor.tenorType.toString) match {
      case (Tenor.ON, _) => NamedMaturity.ON
      case (Tenor.SN, _) => NamedMaturity.SN
      case (tenor, MaturityType.Parse(maturityType)) => RelativeMaturity(tenor.value, maturityType)
    }
  }
  implicit def enrichUOM(uom: UOM) = new {
    def toCurrency = enrichFundamentalUOM(toEDM).toCurrency
    def toEDM = starlingUomToEdmUom(uom)
  }
  implicit def enrichPercentage(percentage: Percentage) = new {
    def toEDM = EPercentage(Some(percentage.value))
  }

  implicit def enrichEDMQuantity(q: EDMQuantity) = new {
    def fromEDM = fromEDMQuantity(q)
  }
  implicit def enrichEDMDate(date: com.trafigura.edm.shared.types.Date) = new {
    def fromEDM = Day.fromLocal(date.datex)
  }
  implicit def enrichEDMDateRange(dateRange: com.trafigura.edm.shared.types.DateRange) = new {
    def fromEDM = new SimpleDateRange(startDay, endDay)
    def contains(date: com.trafigura.edm.shared.types.Date) = fromEDM.contains(date.fromEDM)
    def startDay = Day.fromLocal(dateRange.startDate)
    def endDay = Day.fromLocal(dateRange.endDate)
  }
  implicit def enrichFundamentalUOM(uom: com.trafigura.edm.shared.types.FundamentalUOM) = new {
    def fromEDM = edmToStarlingUomSymbol(uom).asUOM
    def toCurrency: ECurrency = ECurrency().update(_.name = uom.name)
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
        fundamental = starlingUomSymbolToEdmUom.getOrElse(starlingUOMSymbol, FundamentalUOM(starlingUOMSymbol.toString))
       )
    }.toList

    EDMQuantity(Some(q.value), CompoundUOM(unitComponents))
  }

  val starlingUomSymbolToEdmUom = Map(
    gbp -> "GBP",
    usd -> "USD",
    jpy -> "JPY",
    cny -> "RMB",
    TONNE_SYMBOL -> "MTS",
    POUND_SYMBOL -> "LBS"
  ).mapValues(FundamentalUOM(_))

  val starlingUomToEdmUom: Map[UOM, FundamentalUOM] = starlingUomSymbolToEdmUom.mapKeys(_.asUOM)
  val edmToStarlingUomSymbol: Map[FundamentalUOM, UOMSymbol] = starlingUomSymbolToEdmUom.map(_.swap)
}
