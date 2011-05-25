package starling.trade

import java.io.Serializable
import starling.utils.StarlingObject

abstract class TradeSystem(val name : String, val shortCode : String) extends StarlingObject with Serializable{
  override def toString = name
}
case class TradeID(id : String, tradeSystem : TradeSystem) {
  override def toString = id
  val name = tradeSystem.shortCode + id
}

object TradeID {
  def apply(id: Int, tradeSystem : TradeSystem) = {
    new TradeID("" + id, tradeSystem)
  }
}
