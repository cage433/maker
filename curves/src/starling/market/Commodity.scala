package starling.market

import starling.quantity.UOM._
import starling.market.Market._
import starling.quantity.{Conversions, UOM, Quantity}
import starling.utils.StarlingObject

class Commodity {
  lazy val representativeMarket = Commodity.standardFuturesMarket(this)
  lazy val standardFuturesUOM = Commodity.standardFuturesUOM(this)
  def toStandardFuturesLots(position : Quantity) : Quantity = {
    implicit val conv = conversions
    
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

  def conversions = Conversions.default

  lazy val name = {
    val className = getClass.getName.substring(getClass.getName.lastIndexOf(".") + 1)
    if(className.last == '$')
      className.substring(0, className.length-1)
    else
      className
  }
  override def toString = name
}

/**
 * Market class for a nasty Trinity hack in ForwardCurve
 */
sealed abstract class MetalCommodity extends Commodity
object Gold extends MetalCommodity
object Lead extends MetalCommodity
object Silver extends MetalCommodity
object Aluminium extends MetalCommodity
object NASAAC extends MetalCommodity
object AluminiumAlloy extends MetalCommodity
object Cobalt extends MetalCommodity
object Copper extends MetalCommodity
object Nickel extends MetalCommodity
object Rubber extends Commodity
object Zinc extends MetalCommodity
object Tin extends MetalCommodity
object Palladium extends MetalCommodity
object Steel extends MetalCommodity
object IronOre extends MetalCommodity
object Platinum extends MetalCommodity
object Freight extends Commodity
object NatGas extends Commodity
object Indium extends Commodity
object Molybdenum extends Commodity
object Ferromolybdenum extends Commodity

sealed abstract class OilCommodity (
  private val barrelsPerTonneValue : Double
)
  extends Commodity
{
  override def conversions = new Conversions(super.conversions.conversions ++ Map(BBL / MT -> barrelsPerTonneValue))
}

object WTI extends OilCommodity(7.62)
object Brent extends OilCommodity(7.57)
object Urals extends OilCommodity(7.284)
object HeatingOil extends OilCommodity(7.45)
object PalmOil extends OilCommodity(7.05)
object DubaiCrude extends OilCommodity(7.25)

case class Crude(conversion: Double) extends OilCommodity(conversion)
case class GasOil(conversion: Double) extends OilCommodity(conversion)
case class Gasoline(conversion: Double) extends OilCommodity(conversion)
case class FuelOil(conversion: Double) extends OilCommodity(conversion)
case class JetKero(conversion: Double) extends OilCommodity(conversion)
case class Naphtha(conversion: Double) extends OilCommodity(conversion)

object Commodity{

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
      case IronOre => IRON_ORE
      case Platinum => COMEX_PLATINUM
      case Freight => BALTIC_PANAMAX
      case WTI => NYMEX_WTI
      case _:Gasoline => NYMEX_GASOLINE
      case Brent => ICE_BRENT
      case _:GasOil => ICE_GAS_OIL
      case HeatingOil => NYMEX_HEATING
      case _:FuelOil => NYMEX_SINGAPORE_FUEL_OIL
      case NatGas => NYMEX_NAT_GAS
      case PalmOil => MDEX_CRUDE_PALM_OIL
      case DubaiCrude => PLATTS_DUBAI
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
      case g:Gasoline => NYMEX_GASOLINE_LOTS
      case Brent => ICE_BRENT_LOTS
      case _:GasOil => ICE_GAS_OIL_LOTS
      case HeatingOil => NYMEX_HEATING_LOTS
      case _:FuelOil => NYMEX_SINGAPORE_FUEL_OIL_LOTS
      case NatGas => NYMEX_NATGAS_LOTS_LOTS
      case DubaiCrude => DUBAI_CRUDE_LOTS
   }
  }

  private lazy val CommodityNameToCommodity = Map() ++ markets.map(m => (m.commodity.name -> m.commodity))
  def fromName(name:String) = fromNameOption(name).getOrElse(throw new Exception("No such commodity"))
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
