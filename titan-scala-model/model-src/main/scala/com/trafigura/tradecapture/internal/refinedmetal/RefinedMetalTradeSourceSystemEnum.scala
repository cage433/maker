/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from enum_scala.rb

package com.trafigura.tradecapture.internal.refinedmetal

object RefinedMetalTradeSourceSystemEnum {
  def forName(name : String) : RefinedMetalTradeSourceSystemEnum = {
    name match {
      
        case "Neptune" => NeptuneRefinedMetalTradeSourceSystem
      
        case "Titan" => TitanRefinedMetalTradeSourceSystem
      
      case _ => null
    }
  }
}
abstract class RefinedMetalTradeSourceSystemEnum() {
  def toJson : String 
}

  object NeptuneRefinedMetalTradeSourceSystem extends RefinedMetalTradeSourceSystemEnum {
    def toJson = "Neptune"
  }

  object TitanRefinedMetalTradeSourceSystem extends RefinedMetalTradeSourceSystemEnum {
    def toJson = "Titan"
  }


