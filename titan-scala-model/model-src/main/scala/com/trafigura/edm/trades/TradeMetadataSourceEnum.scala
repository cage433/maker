/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from enum_scala.rb

package com.trafigura.edm.trades

object TradeMetadataSourceEnum {
  def forName(name : String) : TradeMetadataSourceEnum = {
    name match {
      
        case "Neptune" => NeptuneTradeMetadataSource
      
        case "Titan" => TitanTradeMetadataSource
      
      case _ => null
    }
  }
}
abstract class TradeMetadataSourceEnum() {
  def toJson : String 
}

  object NeptuneTradeMetadataSource extends TradeMetadataSourceEnum {
    def toJson = "Neptune"
  }

  object TitanTradeMetadataSource extends TradeMetadataSourceEnum {
    def toJson = "Titan"
  }


