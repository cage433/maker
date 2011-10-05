/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import org.jboss.resteasy.client._

import com.trafigura.tradecapture.internal.refinedmetal._


/**
 * Locator for finding MarketLotSizeService instances.
 */
trait MarketLotSizeServiceLocator {
  /**
   * Provides a proxy for the given operation to run.
   */
  def proxy:MarketLotSizeService
}

class LocatableMarketLotSizeService(val locator:MarketLotSizeServiceLocator) extends MarketLotSizeService {
  
    def getMarketLotSizes(): List[com.trafigura.tradecapture.internal.refinedmetal.MarketLotSize] =
      locator.proxy.getMarketLotSizes()
  
}