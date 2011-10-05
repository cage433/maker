/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_json.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradecapture.internal.refinedmetal._

import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradinghub.support._

// JSON support for TrafficHubService

object TrafficHubServiceJSON extends JSONServiceDispatcher[TrafficHubService] {
  def serviceName = "TradeCapture.Internal.RefinedMetalReferenceDataService.TrafficHubService"

  def decodeOperationName(jsonName:String) = {
    jsonName match {
      case "GetTrafficHubs" => "GetTrafficHubs"
      
    }
  }

  def parseParameters(operation:String, params: JSONObject) = {
    operation match {
      
      case "GetTrafficHubs" =>
        
          Map()
        
      
    }
  }

  def invoke(target: TrafficHubService, operation: String, params: Map[String, Any]): Any = {
    operation match {
      
      case "GetTrafficHubs" =>
        target.getTrafficHubs()
      
    }
  }

  def addResponse(operation:String, jsonResponse:JSONObject, response:Any) = {
    val differentiator = new SerialisationHelper
    operation match {
      
      case "GetTrafficHubs" =>
        jsonResponse.putOpt("result", new org.codehaus.jettison.json.JSONArray(if(response.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub]] == null) new java.util.ArrayList() else java.util.Arrays.asList(response.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub]].map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)))
      
    }
  }

  
  def serializeGetTrafficHubs(): JSONObject = {
    val differentiator = new SerialisationHelper
    val params = new JSONObject
    
    params
  }
  

  class Proxy(val handler: (String, JSONObject) => JSONObject) extends TrafficHubService {
  
    def getTrafficHubs(): List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub] = {
        val request = new JSONObject
        request.put("method", "GetTrafficHubs")
        
        var routing = null
        val response = handler(routing, request)
        if (response == null) {
          null.asInstanceOf[List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub]]   // Doesn't work well with primitive types, but we should only see null in cases like discover anyway
        } else {
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(response.opt("result"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.TrafficHub.fromJson(o, cache))) })
        }
    }
  
  }
}

