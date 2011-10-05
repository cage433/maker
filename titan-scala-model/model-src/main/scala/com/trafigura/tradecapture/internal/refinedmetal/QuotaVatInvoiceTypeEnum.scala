/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from enum_scala.rb

package com.trafigura.tradecapture.internal.refinedmetal

object QuotaVatInvoiceTypeEnum {
  def forName(name : String) : QuotaVatInvoiceTypeEnum = {
    name match {
      
        case "Issued" => IssuedQuotaVatInvoiceType
      
        case "Received" => ReceivedQuotaVatInvoiceType
      
      case _ => null
    }
  }
}
abstract class QuotaVatInvoiceTypeEnum() {
  def toJson : String 
}

  object IssuedQuotaVatInvoiceType extends QuotaVatInvoiceTypeEnum {
    def toJson = "Issued"
  }

  object ReceivedQuotaVatInvoiceType extends QuotaVatInvoiceTypeEnum {
    def toJson = "Received"
  }


