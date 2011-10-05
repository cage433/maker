/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_json.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradecapture.internal.refinedmetal._

import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradinghub.support._

// JSON support for PaymentTermsService

object PaymentTermsServiceJSON extends JSONServiceDispatcher[PaymentTermsService] {
  def serviceName = "TradeCapture.Internal.RefinedMetalReferenceDataService.PaymentTermsService"

  def decodeOperationName(jsonName:String) = {
    jsonName match {
      case "GetPaymentTerms" => "GetPaymentTerms"
      
    }
  }

  def parseParameters(operation:String, params: JSONObject) = {
    operation match {
      
      case "GetPaymentTerms" =>
        
          Map()
        
      
    }
  }

  def invoke(target: PaymentTermsService, operation: String, params: Map[String, Any]): Any = {
    operation match {
      
      case "GetPaymentTerms" =>
        target.getPaymentTerms()
      
    }
  }

  def addResponse(operation:String, jsonResponse:JSONObject, response:Any) = {
    val differentiator = new SerialisationHelper
    operation match {
      
      case "GetPaymentTerms" =>
        jsonResponse.putOpt("result", new org.codehaus.jettison.json.JSONArray(if(response.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms]] == null) new java.util.ArrayList() else java.util.Arrays.asList(response.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms]].map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)))
      
    }
  }

  
  def serializeGetPaymentTerms(): JSONObject = {
    val differentiator = new SerialisationHelper
    val params = new JSONObject
    
    params
  }
  

  class Proxy(val handler: (String, JSONObject) => JSONObject) extends PaymentTermsService {
  
    def getPaymentTerms(): List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms] = {
        val request = new JSONObject
        request.put("method", "GetPaymentTerms")
        
        var routing = null
        val response = handler(routing, request)
        if (response == null) {
          null.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms]]   // Doesn't work well with primitive types, but we should only see null in cases like discover anyway
        } else {
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(response.opt("result"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms.fromJson(o, cache))) })
        }
    }
  
  }
}

