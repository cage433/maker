package starling.curves.stress

import starling.market.Commodity
import starling.quantity.Quantity
import starling.curves.{ForwardPriceKey, PerturbedAtomicEnvironment, AtomicDatumKey, AtomicEnvironment}

case class CommodityPriceStress(originalEnv: AtomicEnvironment, commodity: Commodity, commodityDP: Quantity)
  extends PerturbedAtomicEnvironment(originalEnv) {

  def applyWithShiftIfAppropriate(key: AtomicDatumKey): Any = {
    key match {
      case ForwardPriceKey(market, _, _) => {
        val dP = market.commodity match {
          case `commodity` => commodityDP
          case other => {
            market.convert(commodityDP, market.priceUOM) match {
              case Some(q) => q
              case None => {
                // If we don't know how much to shift the environment by then don't shift at all.
                Quantity(0, market.priceUOM)
              }
            }
          }
        }
        originalEnv.quantity(key) + dP
      }
      case _ => originalEnv(key)
    }
  }

  def makeCopyWithNewChildEnvironment(newChildEnv: AtomicEnvironment) = {
    copy(originalEnv = newChildEnv)
  }
}