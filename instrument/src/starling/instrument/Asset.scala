package starling.instrument

import starling.daterange.Day
import starling.quantity.{UOM, Quantity}
import starling.curves.Environment
import starling.market.{CommodityMarket}

object Asset {
  def estimatedCash(settlementDay:Day, amount:Quantity, mtm:Quantity) = {
    val trueAmount = amount.inBaseCurrency
    val trueMtm = mtm.inBaseCurrency
    Asset(false, trueAmount.uom, settlementDay, trueAmount, trueMtm)
  }
  def estimatedCash(settlementDay:Day, amount:Quantity, env:Environment) = cash(false, settlementDay, amount, env)
  def knownCash(settlementDay:Day, amount:Quantity, env:Environment) = cash(true, settlementDay, amount, env)

  private def cash(known:Boolean, settlementDay:Day, amount:Quantity, env:Environment) = {
    val trueAmount = amount.inBaseCurrency
    val discount = if (settlementDay > env.marketDay.day) {
      env.discount(trueAmount.uom, settlementDay)
    } else {
      new Quantity(1)
    }
    Asset(known, trueAmount.uom.toString, settlementDay, trueAmount, trueAmount * discount)
  }

  def estimatedPhysical(market:String, settlementDay:Day, amount:Quantity, mtm:Quantity) =
    Asset(false, market.toString, settlementDay, amount, mtm)

  def knownPhysical(market: CommodityMarket, deliveryDay:Day, amount:Quantity, env:Environment) = {
    val stuffMtm = {
      if (env.marketDay < deliveryDay.endOfDay) {
        val price = env.forwardPrice(market, deliveryDay)
        val discount = env.discount(market.currency, deliveryDay)
        price * amount * discount
      } else {
        amount * env.forwardPrice(market, env.marketDay.day)
      }
    }
    Asset(true, market, deliveryDay, amount, stuffMtm)
  }
}

case class Asset(known:Boolean, assetType:AnyRef, settlementDay:Day, amount:Quantity, mtm:Quantity) {
  def *(volume:Double) = copy(amount = amount*volume, mtm = mtm*volume)
  def inCCY(env : Environment, ccy : UOM) = {
    val mtmInCCY = mtm in ccy match {
      case Some(mtmInCCY) => mtmInCCY
      case None => {
        val fXRate = env.spotFXRate(ccy, mtm.uom)
        (mtm * fXRate) inUOM ccy
      }
    }
    copy(mtm = mtmInCCY)
  }
}

case class Assets(assets:List[Asset]) {
  def mtm(env: Environment, ccy: UOM) = Quantity.sum(Quantity(0, ccy) :: assets.map(_.inCCY(env, ccy).mtm))
  def ++ (other:Assets) = Assets(assets ::: other.assets)
  def * (volume:Double) = Assets(assets.map(_ * volume))
}

object Assets {
  def apply(assets : Asset*) = new Assets(assets.toList)
}
