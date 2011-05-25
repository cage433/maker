package starling.instrument

import starling.daterange.Day
import starling.quantity.{UOM, Quantity}
import starling.market.ForwardMarket
import starling.curves.Environment
object Asset {
  def estimatedCash(settlementDay:Day, amount:Quantity, mtm:Quantity) = Asset(false, false, amount.uom, settlementDay, amount, mtm)
  def estimatedCash(settlementDay:Day, amount:Quantity, env:Environment) = cash(false, settlementDay, amount, env)
  def knownCash(settlementDay:Day, amount:Quantity, env:Environment) = cash(true, settlementDay, amount, env)

  private def cash(known:Boolean, settlementDay:Day, amount:Quantity, env:Environment) = {
    val discount = if (settlementDay > env.marketDay.day) {
      env.discount(amount.uom, settlementDay)
    } else {
      1
    }
    Asset(known, settlementDay < env.marketDay.day, amount.uom.toString, settlementDay, amount, amount * discount)
  }

  def estimatedPhysical(market:String, settlementDay:Day, amount:Quantity, mtm:Quantity) =
    Asset(false, false, market.toString, settlementDay, amount, mtm)

  def knownPhysical(market:ForwardMarket, deliveryDay:Day, amount:Quantity, env:Environment) = {
    val stuffMtm = {
      if (env.marketDay < deliveryDay.endOfDay) {
        var price = env.forwardPrice(market, market.underlying(deliveryDay))
        val discount = env.discount(market.currency, market.settlementDay(deliveryDay))
        price * amount * discount
      } else {
        amount * env.forwardPrice(market, market.underlying(env.marketDay.day))
      }
    }
    Asset(true, deliveryDay < env.marketDay.day, market, deliveryDay, amount, stuffMtm)
  }
}

case class Asset(known:Boolean, isPast:Boolean, market:AnyRef, settlementDay:Day, amount:Quantity, mtm:Quantity) {
  def *(volume:Double) = Asset(known, isPast, market, settlementDay, amount*volume, mtm*volume)
  def copyMtm(m:Quantity) = copy(mtm=m)
}

case class Assets(assets:List[Asset]) {
  def mtm(env: Environment, ccy: UOM) = Quantity.sum(Quantity(0, ccy) :: assets.map(asset => {
    val fXRate = env.spotFXRate(ccy, asset.mtm.uom)
    asset.mtm * fXRate
  }))
  def ++ (other:Assets) = Assets(assets ::: other.assets)
  def * (volume:Double) = Assets(assets.map(_ * volume))
}

object Assets {
  private val empty = new Assets(List())
  def apply() = empty
  def apply(a:Asset) = new Assets(List(a))
  def apply(a:Asset, b:Asset) = new Assets(List(a, b)) //varargs?
}
