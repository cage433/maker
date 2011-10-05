/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from enum_scala.rb

package com.trafigura.edm.logistics.inventory

object EDMInventoryItemStatusEnum {
  def forName(name : String) : EDMInventoryItemStatusEnum = {
    name match {
      
        case "Expected" => ExpectedEDMInventoryItemStatus
      
        case "Split" => SplitEDMInventoryItemStatus
      
        case "Received" => ReceivedEDMInventoryItemStatus
      
        case "InTransit" => InTransitEDMInventoryItemStatus
      
        case "ProvisionalDelivery" => ProvisionalDeliveryEDMInventoryItemStatus
      
        case "FinalDelivery" => FinalDeliveryEDMInventoryItemStatus
      
        case "Cancelled" => CancelledEDMInventoryItemStatus
      
      case _ => null
    }
  }
}
abstract class EDMInventoryItemStatusEnum() {
  def toJson : String 
}

  object ExpectedEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "Expected"
  }

  object SplitEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "Split"
  }

  object ReceivedEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "Received"
  }

  object InTransitEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "InTransit"
  }

  object ProvisionalDeliveryEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "ProvisionalDelivery"
  }

  object FinalDeliveryEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "FinalDelivery"
  }

  object CancelledEDMInventoryItemStatus extends EDMInventoryItemStatusEnum {
    def toJson = "Cancelled"
  }


