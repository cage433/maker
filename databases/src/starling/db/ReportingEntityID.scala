package starling.db

import starling.daterange._
import starling.instrument._
import starling.trade.{TradeID, TradeSystem}

case object EAITradeSystem extends TradeSystem("EAI", "eai")
case object IntradayTradeSystem extends TradeSystem("Intraday", "int")
case object RefinedAssignmentTradeSystem extends TradeSystem("Refined Assignment", "ra")
case object RefinedFixationTradeSystem extends TradeSystem("Refined Fixation", "rf")
case object TitanTradeSystem extends TradeSystem("Titan", "ti")

object TradeSystems {
  // note: intraday is missing here because it isn't a "primary" trade store - it's desgined to
  // be combined with other trade stores to provide the difference between real-time position
  // and close of book the previous day.
  val systems = List(EAITradeSystem, RefinedAssignmentTradeSystem, RefinedFixationTradeSystem, TitanTradeSystem)
  private val allSystems = IntradayTradeSystem :: systems

  def fromName(name:String) = allSystems.find(_.name == name) match {
    case Some(system) => system
    case None => throw new Exception("No system found with name " + name + " in " + systems)
  }
}

object TradeIDParser {
  def parse(text:String) = {
    if (text.startsWith("eai")) {
      TradeID(text.substring(3), EAITradeSystem)
    } else if (text.startsWith("int")) {
      TradeID(text.substring(3), IntradayTradeSystem)
    } else {
      throw new Exception("Can't parse '" + text + "'")
    }
  }
}



