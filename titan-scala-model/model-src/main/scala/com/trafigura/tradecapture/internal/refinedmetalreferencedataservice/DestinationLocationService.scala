/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradecapture.internal.refinedmetal._

import com.trafigura.tradinghub.support._
import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradinghub.support._


/**
 * 
 */

trait DestinationLocationService extends ModelService {
  override def serviceNames = super.serviceNames ++ Seq("DestinationLocationService")
  override def serviceKeys = super.serviceKeys ++ Seq("DestinationLocationService")

  
    def getDestinationLocations(): List[com.trafigura.tradecapture.internal.refinedmetal.DestinationLocation]
  
}
