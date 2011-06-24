package starling.market

import starling.quantity.UOM._
import starling.market.Market._
import starling.quantity.{Conversions, UOM, Quantity}
import starling.utils.ImplicitConversions._
import net.sf.cglib.core.CollectionUtils
import starling.utils.CollectionUtils


class Commodity {
  lazy val representativeMarket = Commodity.standardFuturesMarket(this)
  lazy val standardFuturesUOM = Commodity.standardFuturesUOM(this)
  def toStandardFuturesLots(position : Quantity)(implicit conv: Conversions = Conversions.default): Quantity = {
    position in representativeMarket.uom match {
      case Some(positionInMarketUOM) => {
        representativeMarket.lotSize match {
          case Some(ls) => Quantity((positionInMarketUOM / ls).value, standardFuturesUOM)
          case None => Quantity.NULL // FIXME this isn't a great thing to do
        }
      }
      case None => {
        throw new Exception("Couldn't convert " + position + " to "  + representativeMarket.uom + " of market " + representativeMarket)
      }
    }
  }

  /**
   * Commodities are equal if their names are the same. FuelOil is FuelOil regardless of
   * what the conversion is. The conversion shouldn't be stored in Commodity, it should be separate, but
   * I won't refactor for the moment as it might clash badly with Alex's market refactor.
   */
  override def equals(obj: Any) = obj match {
    case c:Commodity => c.name == this.name
    case _ => false
  }

  override def hashCode = name.hashCode

  lazy val name = getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1).stripSuffix("$")
  override def toString = name
}

sealed abstract class MetalCommodity extends Commodity
object Gold extends MetalCommodity
object Lead extends MetalCommodity
object Silver extends MetalCommodity
object Aluminium extends MetalCommodity
object Alumina extends MetalCommodity
object NASAAC extends MetalCommodity
object AluminiumAlloy extends MetalCommodity
object Cobalt extends MetalCommodity
object Copper extends MetalCommodity
object Nickel extends MetalCommodity
object Zinc extends MetalCommodity
object Tin extends MetalCommodity
object Palladium extends MetalCommodity
object Steel extends MetalCommodity
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

object Commodity{
  lazy val metalsCommodities = List(Gold, Lead, Silver, Aluminium, Alumina, NASAAC, AluminiumAlloy, Copper, Cobalt,
    Nickel, Zinc, Tin, Palladium, Steel, IronOre, Platinum, Indium, Molybdenum, Ferromolybdenum)

  lazy val oilCommodities = List(WTI, Brent, HeatingOil, Crude, GasOil, Gasoline, FuelOil, JetKero, Naphtha, VegetableOil)

  lazy val gasCommodities = List(Propane, Butane, Ethane, NatGas)

  lazy val otherCommodities = List(Coal, Rubber, Freight)

  lazy val all = metalsCommodities ::: oilCommodities ::: gasCommodities ::: otherCommodities

  def standardFuturesMarket(commodity : Commodity) : FuturesMarket = {
    commodity match {
      case Gold => COMEX_GOLD
      case Lead => LME_LEAD
      case Silver => COMEX_SILVER
      case Aluminium => LME_ALUMINIUM
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
   }
  }

  def standardFuturesUOM(commodity : Commodity) : UOM = {
    commodity match {
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
   }
  }

  private lazy val CommodityNameToCommodity = Map() ++ all.map(c => (c.name -> c))
  def fromName(name:String) = fromNameOption(name).getOrElse(throw new Exception("No such commodity" + name))
  def fromNameOption(name: String) = CommodityNameToCommodity.get(name)

  def hasStandardFuturesMarket(commodity:Commodity) = {
    try {
      standardFuturesMarket(commodity)
      true
    } catch {
      case me:MatchError => false
    }
  }
}
