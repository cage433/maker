package starling.titan

import starling.quantity.UOMSymbol._
import starling.utils.ImplicitConversions._
import starling.quantity.{UOMSymbol, Percentage, UOM, Quantity}
import com.trafigura.edm.common.units.{Currency => TitanCurrency, Date => TitanDate,
                                       Percentage => TitanPercentage, Quantity => TitanQuantity, EQuantity,
                                       CompoundUOM, UnitComponent, FundamentalUOM}
import com.trafigura.services._
import com.trafigura.services.marketdata.Maturity
import starling.daterange.{DateRange, Tenor, SimpleDateRange, Day}
import com.trafigura.edm.common.types.datespecifications.{DateRange => TitanDateRange}
import starling.utils.Pattern.Extractor
import starling.db.{MarketDataStore, SnapshotID}
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import com.trafigura.edm.logistics.inventory.InventoryItem
import starling.instrument.Trade
import starling.instrument.physical.PhysicalMetalAssignment

case class InvalidUomException(msg : String) extends Exception(msg)

object EDMConversions {

  implicit def toRichEDMInventoryItem(inv : InventoryItem) = RichEDMInventoryItemForTitan(inv)
  implicit def toRichQuantity(q : Quantity) = RichQuantityForTitan(q)
  implicit def toRichTitanTenor(tenor : Tenor) = RichTitanTenorForTitan(tenor)
  implicit def toRichUOM(uom : UOM) = RichUOMForTitan(uom)
  implicit def toRichPercentage(percentage : Percentage) = RichPercentageForTitan(percentage)
  implicit def toRichDay(day: Day) = RichDayForTitan(day)
  implicit def toRichSnapshotID(snapshotID: SnapshotID) = RichSnapshotIDForTitan(snapshotID)
  implicit def toRichSerializableDate(date: TitanSerializableDate) = RichTitanSerializableDate(date)
  implicit def toRichSerializableDateRange(dateRange: TitanSerializableDateRange) = RichTitanSerializableDateRange(dateRange)
  implicit def toRichSerializableCurrency(currency: TitanSerializableCurrency) = RichTitanSerializableCurrency(currency)
  implicit def toRichTitanMarketDataIdentifier(marketDataID : TitanMarketDataIdentifier) = RichTitanMarketDataIdentifier(marketDataID)
  implicit def toRichTitanMarketDataIdentifier(snapshotID : TitanSnapshotIdentifier) = RichTitanMarketDataIdentifierForTitan(snapshotID)
  implicit def toRichTitanQuantity(q: TitanQuantity) = RichTitanQuantity(q)
  implicit def toRichTitanDate(date: TitanDate) = RichTitanDate(date)
  implicit def toRichTitanDateRange(dateRange: TitanDateRange) = RichTitanDateRange(dateRange)
  implicit def toRichFundamentalUOM(uom: FundamentalUOM) = RichFundamentalUOMForTitan(uom)
  implicit def enhanceStarlingTrade(trade : Trade) = RichStarlingTradeForTitan(trade)

  // Starling implicits
  case class RichEDMInventoryItemForTitan(inv : InventoryItem) {
    val id = inv.oid.contents.toString
  }

  case class RichQuantityForTitan(q: Quantity) {
    def toTitan = toTitanQuantity(q)
    def toSerializable = toTitanSerializableQuantity(q)
    def toSerializablePercentage = TitanSerializablePercentage(q.checkedValue(UOM.PERCENT))
  }

  case class RichTitanTenorForTitan(tenor: Tenor) {
    def toTitan: Maturity = Maturity.get(tenor.toString)
  }

  case class RichUOMForTitan(uom : UOM) {
    def titanCurrency: Option[TitanCurrency] = toTitan.map(edm => TitanCurrency().update(_.name = edm.name))
    def serializableCurrency: Option[TitanSerializableCurrency] =
      starlingUomToEdmUomName.get(uom).map(fuom => TitanSerializableCurrency(fuom))
    def toTitan: Option[FundamentalUOM] = starlingUomToEdmUomName.get(uom).map(n => new FundamentalUOM{ name = n})
  }

  case class RichPercentageForTitan(percentage : Percentage) {
    def toTitan = TitanPercentage(Some(percentage.value))
    def toSerializable = TitanSerializablePercentage(percentage.value)
  }

  case class RichDayForTitan(day : Day) {
    def toTitan = TitanDate(day.toLocalDate)
    def toSerializable = TitanSerializableDate(day.toLocalDate)
  }

  case class RichSnapshotIDForTitan(snapshotID : SnapshotID) {
    def toSerializable = TitanSnapshotIdentifier(snapshotID.id.toString)
  }

  // 'Serializable' implicits
  case class RichTitanSerializableDate(date : TitanSerializableDate) {
    def fromSerializable = Day.fromLocalDate(date.value)
    def toDateRange = new TitanSerializableDateRange(date.value, date.value)
  }

  case class RichTitanSerializableDateRange(dateRange : TitanSerializableDateRange) {
    def fromSerializable = DateRange(Day.fromLocalDate(dateRange.start), Day.fromLocalDate(dateRange.end))
  }

  case class RichTitanSerializableCurrency(currency : TitanSerializableCurrency) {
    def fromSerializable = UOM.asUOM(edmToStarlingUomSymbol(currency.name))
  }

  val UOMToTitanCurrency: Extractor[UOM, TitanSerializableCurrency] = Extractor.from[UOM](_.serializableCurrency)
  val StringToTitanCurrency: Extractor[Any, TitanSerializableCurrency] = UOM.Currency.andThen(UOMToTitanCurrency)

  case class RichTitanMarketDataIdentifier(marketDataID : TitanMarketDataIdentifier) {
    def intId = marketDataID.snapshotIdentifier.id.toInt
    def toTitanDate = TitanSerializableDate(marketDataID.observationDay.toJodaLocalDate)
    def toDay = marketDataID.observationDay
    def fromTitan(marketDataStore: MarketDataStore): Option[SnapshotID] = marketDataStore.snapshotFromID(intId)
  }

  case class RichTitanMarketDataIdentifierForTitan(snapshotID : TitanSnapshotIdentifier) {
    def intId = snapshotID.id.toInt
    def fromTitan(marketDataStore: MarketDataStore): Option[SnapshotID] = marketDataStore.snapshotFromID(intId)
  }

  // Titan implicits
  case class RichTitanQuantity(q: TitanQuantity) {
    def fromTitan = fromTitanQuantity(q)
  }

  case class RichTitanDate(date : TitanDate) {
    def fromTitan = Day.fromLocalDate(date.value)
  }

  case class RichTitanDateRange(dateRange: TitanDateRange) {
    def fromTitan = new SimpleDateRange(startDay, endDay)
    def contains(date: TitanDate) = fromTitan.contains(date.fromTitan)
    def startDay = Day.fromLocalDate(dateRange.start.dateValue)
    def endDay = Day.fromLocalDate(dateRange.finish.dateValue)
  }

  case class RichFundamentalUOMForTitan(uom: FundamentalUOM) {
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
      case None => throw new Exception("Invalid quantity - no amount in EDM Quantity " + q)
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

  case class RichStarlingTradeForTitan(trade : Trade) {
    def titanTradeID : Option[String] = trade.attributes match {
      case tta : TitanTradeAttributes => Some(tta.titanTradeID)
      case _ => None
    }
    def titanInventoryID : Option[String] = trade.tradeable match {
      case pma : PhysicalMetalAssignment => Some(pma.inventoryID)
      case _ => None
    }
  }

  val starlingCurrencyToEdmCurrency = edmCurrencies.toMapWithValues(_.name.toString) + (UOMSymbol.cny → "RMB")

  implicit def toStarlingCurrency(edmCurrency : TitanCurrency) : UOM = UOM.fromString(edmCurrency.name)

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
