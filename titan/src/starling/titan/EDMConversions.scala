package starling.titan

import starling.quantity.UOMSymbol._
import starling.utils.ImplicitConversions._
import starling.quantity.{UOMSymbol, Percentage, UOM, Quantity}
import com.trafigura.edm.shared.types.{Currency => TitanCurrency, Date => TitanDate,
                                       Percentage => TitanPercentage, Quantity => TitanQuantity, EQuantity,
                                       CompoundUOM, UnitComponent, FundamentalUOM}
import com.trafigura.services._
import com.trafigura.services.marketdata.Maturity
import starling.daterange.{DateRange, Tenor, SimpleDateRange, Day}
import com.trafigura.edm.common.types.datespecifications.{DateRange => TitanDateRange}
import starling.utils.Pattern.Extractor
import starling.db.{MarketDataStore, SnapshotID}
import starling.gui.api.{SnapshotMarketDataVersion, SpecificMarketDataVersion, MarketDataVersion}
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import com.trafigura.edm.logistics.inventory.EDMInventoryItem
import starling.instrument.Trade

case class InvalidUomException(msg : String) extends Exception(msg)

object EDMConversions {
  implicit def enrichEDMInventoryItem(inv : EDMInventoryItem) = new {
    val id = inv.oid.contents.toString
  }
  // Starling implicits
  implicit def enrichQuantity(q: Quantity) = new {
    def toTitan = toTitanQuantity(q)
    def toSerializable = toTitanSerializableQuantity(q)
  }

  implicit def enrichTenor(tenor: Tenor) = new {
    def toTitan: Maturity = Maturity.get(tenor.toString)
  }

  implicit def enrichUOM(uom: UOM) = new {
    def titanCurrency: Option[TitanCurrency] = toTitan.map(edm => TitanCurrency().update(_.name = edm.name))
    def serializableCurrency: Option[TitanSerializableCurrency] =
      starlingUomToEdmUomName.get(uom).map(fuom => TitanSerializableCurrency(fuom))
    def toTitan: Option[FundamentalUOM] = starlingUomToEdmUomName.get(uom).map(n => new FundamentalUOM{ name = n})
  }

  implicit def enrichPercentage(percentage: Percentage) = new {
    def toTitan = TitanPercentage(Some(percentage.value))
    def toSerializable = TitanSerializablePercentage(percentage.value)
  }

  implicit def enrichDay(day: Day) = new {
    def toTitan = TitanDate(day.toLocalDate)
    def toSerializable = TitanSerializableDate(day.toLocalDate)
  }

  implicit def enrichSnapshotID(snapshotID: SnapshotID) = new {
    def toSerializable = TitanSnapshotIdentifier(snapshotID.id.toString)
  }

  // 'Serializable' implicits
  implicit def enrichSerializableDate(date: TitanSerializableDate) = new {
    def fromSerializable = Day.fromLocalDate(date.value)
    def toDateRange = new TitanSerializableDateRange(date.value, date.value)
  }

  implicit def enrichSerializableDateRange(dateRange: TitanSerializableDateRange) = new {
    def fromSerializable = DateRange(Day.fromLocalDate(dateRange.start), Day.fromLocalDate(dateRange.end))
  }

  implicit def enrichSerializableCurrency(currency: TitanSerializableCurrency) = new {
    def fromSerializable = UOM.asUOM(edmToStarlingUomSymbol(currency.name))
  }

  val UOMToTitanCurrency: Extractor[UOM, TitanSerializableCurrency] = Extractor.from[UOM](_.serializableCurrency)
  val StringToTitanCurrency: Extractor[Any, TitanSerializableCurrency] = UOM.Currency.andThen(UOMToTitanCurrency)

  implicit def enrichTitanMarketDataIdentifier(marketDataID : TitanMarketDataIdentifier) = new {
    def intId = marketDataID.snapshotIdentifier.id.toInt
    def toTitanDate = TitanSerializableDate(marketDataID.observationDay.toJodaLocalDate)
    def toDay = marketDataID.observationDay
    def fromTitan(marketDataStore: MarketDataStore): Option[SnapshotID] = marketDataStore.snapshotFromID(intId)
  }

  implicit def enrichTitanMarketDataIdentifier(snapshotID : TitanSnapshotIdentifier) = new {
    def intId = snapshotID.id.toInt
    def fromTitan(marketDataStore: MarketDataStore): Option[SnapshotID] = marketDataStore.snapshotFromID(intId)
  }

  // Titan implicits
  implicit def enrichTitanQuantity(q: TitanQuantity) = new {
    def fromTitan = fromTitanQuantity(q)
  }

  implicit def enrichTitanDate(date: TitanDate) = new {
    def fromTitan = Day.fromLocalDate(date.value)
  }

  implicit def enrichTitanDateRange(dateRange: TitanDateRange) = new {
    def fromTitan = new SimpleDateRange(startDay, endDay)
    def contains(date: TitanDate) = fromTitan.contains(date.fromTitan)
    def startDay = Day.fromLocalDate(dateRange.start.dateValue)
    def endDay = Day.fromLocalDate(dateRange.finish.dateValue)
  }

  implicit def enrichFundamentalUOM(uom: FundamentalUOM) = new {
    def fromTitan = UOM.asUOM(edmToStarlingUomSymbol(uom.name))
    def titanCurrency: TitanCurrency = TitanCurrency().update(_.name = uom.name)
  }

  implicit def fromTitanQuantity(q : TitanQuantity) : Quantity = {
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

  implicit def fromTitanQuantity(q : EQuantity)(implicit uomIdToSymbolName : Map[Int, String]) : Quantity = {
     // No idea why this is optional in EDM
    val amount = q.amount match {
      case Some(amt) => amt
      case None => throw new Exception("Invalid quantity - no amount")
    }
    
    val uom = UOM.fromSymbolMap(q.uomId match {
      case Some(id) => {
        edmToStarlingUomSymbol.get(uomIdToSymbolName(id)) match {
          case Some(uomSymbol) => List(uomSymbol -> 1).toMap
          case None => throw new InvalidUomException("Id = " + id)
        }
      }
      case _ => throw new InvalidUomException("Missing UOM id")
    })

    Quantity(amount, uom)
  }

  implicit def toTitanQuantity(q : Quantity) : TitanQuantity = {
    val symbolPowers = q.uom.asSymbolMap

    // create edm UOMs, EDM symbol list is GBP, USD, JPY, RMB, MTS, LBS

    val unitComponents = symbolPowers.map{
      case (starlingUOMSymbol, power) => UnitComponent(
        exponent = power,
        fundamental = new FundamentalUOM{name = starlingUomSymbolToEdmUom.getOrElse(starlingUOMSymbol, starlingUOMSymbol.toString)}
       )
    }.toList

    TitanQuantity(Some(q.value), CompoundUOM(unitComponents))
  }

  implicit def toTitanSerializableQuantity(q : Quantity) : TitanSerializableQuantity = {
    val symbolPowers = q.uom.asSymbolMap

    val uomMap = symbolPowers.map{
      case (starlingUOMSymbol, power) => (
        starlingUomSymbolToEdmUom.getOrElse(starlingUOMSymbol, starlingUOMSymbol.toString),
        power
      )
    }.toMap
    TitanSerializableQuantity(q.value, uomMap)
  }

  implicit def enhanceStarlingTrade(trade : Trade) = new {
    def titanTradeID : Option[String] = trade.attributes match {
      case tta : TitanTradeAttributes => Some(tta.titanTradeID)
      case _ => None
    }
  }

  val starlingCurrencyToEdmCurrency = Map(
    aed -> "AED",
    gbp -> "GBP",
    eur -> "EUR",
    zar -> "ZAR",
    usd -> "USD",
    jpy -> "JPY",
    cny -> "RMB"
  )

  val starlingUomSymbolToEdmUom = starlingCurrencyToEdmCurrency ++ Map(
    TONNE_SYMBOL -> "MTS",
    POUND_SYMBOL -> "LBS"

//    LegacyCurrency.Aed -> "AED",
//    LegacyCurrency.Ecb -> "ECB",
//    LegacyCurrency.Eur -> "EUR",
//    LegacyCurrency.Fx1 -> "FX1",
//    LegacyCurrency.Gbp -> "GBP",
//    LegacyCurrency.Itl -> "ITL",
//    LegacyCurrency.Jpy -> "JPY",
//    LegacyCurrency.Rmb -> "RMB",
//    LegacyCurrency.Usd -> "USD",
//    LegacyCurrency.Zar -> "ZAR"
  )
  val starlingUomToEdmUomName: Map[UOM, String] = starlingUomSymbolToEdmUom.mapKeys(UOM.asUOM(_))
  val edmToStarlingUomSymbol: Map[String, UOMSymbol] = starlingUomSymbolToEdmUom.map(_.swap) + ("ECB" -> eur)
}
