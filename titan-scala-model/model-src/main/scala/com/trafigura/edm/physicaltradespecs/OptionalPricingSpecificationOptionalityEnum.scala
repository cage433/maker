/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from enum_scala.rb

package com.trafigura.edm.physicaltradespecs

object OptionalPricingSpecificationOptionalityEnum {
  def forName(name : String) : OptionalPricingSpecificationOptionalityEnum = {
    name match {
      
        case "Buyer" => BuyerOptionalPricingSpecificationOptionality
      
        case "Seller" => SellerOptionalPricingSpecificationOptionality
      
        case "None" => NoneOptionalPricingSpecificationOptionality
      
      case _ => null
    }
  }
}
abstract class OptionalPricingSpecificationOptionalityEnum() {
  def toJson : String 
}

  object BuyerOptionalPricingSpecificationOptionality extends OptionalPricingSpecificationOptionalityEnum {
    def toJson = "Buyer"
  }

  object SellerOptionalPricingSpecificationOptionality extends OptionalPricingSpecificationOptionalityEnum {
    def toJson = "Seller"
  }

  object NoneOptionalPricingSpecificationOptionality extends OptionalPricingSpecificationOptionalityEnum {
    def toJson = "None"
  }


