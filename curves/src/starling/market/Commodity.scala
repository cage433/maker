package starling.market

import starling.quantity.UOM._
import starling.market.Market._
import starling.quantity.UOM
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._
import starling.marketdata.{GradeAreaBenchmarkMarketDataKey, MarketDataKey, CountryBenchmarkMarketDataKey}
import starling.utils.Log


trait Commodity {
  def hasRepresentativeMarket = Commodity.hasStandardFuturesMarket(this)
  lazy val representativeMarket = Commodity.standardFuturesMarket(this)
  lazy val standardFuturesUOM = Commodity.standardFuturesUOM(this)

  /**
   * Commodities are equal if their names are the same. FuelOil is FuelOil regardless of
   * what the conversion is. The conversion shouldn't be stored in Commodity, it should be separate, but
   * I won't refactor for the moment as it might clash badly with Alex's market refactor.
   */
  override def equals(obj: Any) = obj.equalTo[Commodity](_.name == name)
  override def hashCode = name.hashCode
  lazy val name = getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1).stripSuffix("$")
  override def toString = name
}

trait NeptuneCommodity extends MetalCommodity{
  def neptuneCode : String
  def neptuneName : String
  def countryBenchmarkKey: MarketDataKey = CountryBenchmarkMarketDataKey(this)
  def gradeAreaBenchmarkKey: MarketDataKey = GradeAreaBenchmarkMarketDataKey(this)
}

sealed trait MetalCommodity extends Commodity

object Lead extends NeptuneCommodity{
  val neptuneName = "Lead"
  val neptuneCode = "PB"
}
object Aluminium extends NeptuneCommodity{
  val neptuneName = "Primary Aluminium"
  val neptuneCode = "AHD"
}
object NASAAC extends NeptuneCommodity{
  val neptuneName = "NASAAC"
  val neptuneCode = "NAS"
}
object AluminiumAlloy extends NeptuneCommodity{
  val neptuneName = "Aluminium Alloy"
  val neptuneCode = "AA"
}
object Copper extends NeptuneCommodity{
  val neptuneName = "Copper"
  val neptuneCode = "CAD"
}
object Nickel extends NeptuneCommodity{
  val neptuneName = "Nickel"
  val neptuneCode = "NI"
}
object Zinc extends NeptuneCommodity{
  val neptuneName = "Zinc"
  val neptuneCode = "ZN"
}
object Tin extends NeptuneCommodity{
  val neptuneName = "Tin"
  val neptuneCode = "SN"
}
object Steel extends NeptuneCommodity{
  val neptuneName = "Steel"
  val neptuneCode = "STL"
}

object NeptuneCommodity{
  val commodities = List(Lead, Aluminium, NASAAC, AluminiumAlloy, Copper, Nickel, Zinc, Tin, Steel)
  def fromNeptuneName(name : String) = commodities.find(_.neptuneName == name)
  def fromNeptuneCode(code : String) = commodities.find(_.neptuneCode == code)
}
object Gold extends MetalCommodity
object Silver extends MetalCommodity
object Cobalt extends MetalCommodity
object Palladium extends MetalCommodity
object Alumina extends MetalCommodity
object IronOre extends MetalCommodity
object Indium extends MetalCommodity
object Platinum extends MetalCommodity
object Molybdenum extends MetalCommodity
object Ferromolybdenum extends MetalCommodity

object Coal extends Commodity
object Rubber extends Commodity
object Freight extends Commodity

sealed abstract class Gas extends Commodity
object Propane extends Gas
object Butane extends Gas
object Ethane extends Gas
object NatGas extends Gas

sealed abstract class OilCommodity extends Commodity
object WTI extends OilCommodity
object Brent extends OilCommodity
object HeatingOil extends OilCommodity
object Crude extends OilCommodity
object GasOil extends OilCommodity
object Gasoline extends OilCommodity
object FuelOil extends OilCommodity
object JetKero extends OilCommodity
object Naphtha extends OilCommodity
object VegetableOil extends OilCommodity

object Commodity extends Log {
  lazy val metalsCommodities = List(Gold, Lead, Silver, Aluminium, Alumina, NASAAC, AluminiumAlloy, Copper, Cobalt,
    Nickel, Zinc, Tin, Palladium, Steel, IronOre, Platinum, Indium, Molybdenum, Ferromolybdenum)

  lazy val oilCommodities = List(WTI, Brent, HeatingOil, Crude, GasOil, Gasoline, FuelOil, JetKero, Naphtha, VegetableOil)

  lazy val gasCommodities = List(Propane, Butane, Ethane, NatGas)

  lazy val otherCommodities = List(Coal, Rubber, Freight)

  lazy val all = metalsCommodities ::: oilCommodities ::: gasCommodities ::: otherCommodities

  lazy val neptuneCommodities = metalsCommodities.collect{case c : NeptuneCommodity => c}
  lazy val neptuneCommodityFromNeptuneCode : Map[String, NeptuneCommodity] = neptuneCommodities.toMapWithKeys(_.neptuneCode).withDefault{
    code: String => throw new Exception("Unrecognized neptune commodity code " + code + ", known codes are " + neptuneCommodities.map(_.neptuneCode).mkString(", "))
  }

  def neptuneCommodityFromNeptuneName(name : String) = {
    neptuneCommodities.find(_.neptuneName == name) match {
      case nco @ Some(nc) => nco
      case None => {
        log.error("Missing neptune commodity, name %s from \n%s".format(name, neptuneCommodities.mkString("\n")))
        None
      }
    }
  }

  def standardFuturesMarket(commodity : Commodity) : FuturesMarket = commodity match {
    case Gold => COMEX_GOLD
    case Lead => LME_LEAD
    case Silver => COMEX_SILVER
    case Aluminium => LME_ALUMINIUM
    case AluminiumAlloy => LME_ALUMINIUM_ALLOY
    case Copper => LME_COPPER
    case Nickel => LME_NICKEL
    case Zinc => LME_ZINC
    case Tin => LME_TIN
    case Molybdenum => LME_MOLYBDENUM
    case Palladium => COMEX_PALLADIUM
    case Steel => STEEL_REBAR_SHANGHAI
    case Platinum => COMEX_PLATINUM
    case WTI => NYMEX_WTI
    case Gasoline => NYMEX_GASOLINE
    case Brent => ICE_BRENT
    case GasOil => ICE_GAS_OIL
    case HeatingOil => NYMEX_HEATING
    case FuelOil => NYMEX_SINGAPORE_FUEL_OIL
    case NatGas => NYMEX_NAT_GAS
    case NASAAC => LME_NASAAC
    case _ => throw new Exception("Unknown Standard Futures Market for: " + commodity)
  }

  def standardFuturesUOM(commodity : Commodity) : UOM = commodity match {
    case Gold => COMEX_GOLD_LOTS
    case Lead => LME_LEAD_LOTS
    case Silver => COMEX_SILVER_LOTS
    case Aluminium => LME_ALUMINIUM_LOTS
    case Copper => LME_COPPER_LOTS
    case Nickel => LME_NICKEL_LOTS
    case Rubber => SHANGHAI_RUBBER_LOTS
    case Zinc => LME_ZINC_LOTS
    case Tin => LME_TIN_LOTS
    case Molybdenum => LME_MOLYBDENUM_LOTS
    case Palladium => COMEX_PALLADIUM_LOTS
    case Steel => STEEL_REBAR_SHANGHAI_LOTS
    case IronOre => IRON_ORE_LOTS
    case Platinum => COMEX_PLATINUM_LOTS
    case Freight => BALTIC_PANAMAX_LOTS
    case WTI => NYMEX_WTI_LOTS
    case Gasoline => NYMEX_GASOLINE_LOTS
    case Brent => ICE_BRENT_LOTS
    case GasOil => ICE_GAS_OIL_LOTS
    case HeatingOil => NYMEX_HEATING_LOTS
    case FuelOil => NYMEX_SINGAPORE_FUEL_OIL_LOTS
    case NatGas => NYMEX_NATGAS_LOTS_LOTS
    case _ => throw new Exception("Unknown Standard Futures UOM for: " + commodity)
  }

  lazy val CommodityNameToCommodity = all.toMapWithKeys(_.name)
  def fromName(name:String) = fromNameOption(name).getOrElse(throw new Exception("No such commodity" + name))
  def fromNameOption(name: String) = CommodityNameToCommodity.get(name)
  def hasStandardFuturesMarket(commodity:Commodity) = succeeds(standardFuturesMarket(commodity))
}
