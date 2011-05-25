package starling.gui.api

object TradeSystemLabels {
  val TrinityTradeSystem = new TradeSystemLabel("Trinity", "tr") { override def toString = name }
val GalenaTradeSystem = new TradeSystemLabel("Galena", "ga") { override def toString = name }
val EAITradeSystem = new TradeSystemLabel("EAI", "eai") { override def toString = name }
val IntradayTradeSystem = new TradeSystemLabel("Intraday", "int") { override def toString = name }
}