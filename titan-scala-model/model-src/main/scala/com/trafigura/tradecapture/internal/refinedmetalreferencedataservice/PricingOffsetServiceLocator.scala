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
 * Locator for finding PricingOffsetService instances.
 */
trait PricingOffsetServiceLocator {
  /**
   * Provides a proxy for the given operation to run.
   */
  def proxy:PricingOffsetService
}

class LocatablePricingOffsetService(val locator:PricingOffsetServiceLocator) extends PricingOffsetService {
  
    def getPricingOffsets(): List[com.trafigura.tradecapture.internal.refinedmetal.PricingOffset] =
      locator.proxy.getPricingOffsets()
  
}