/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator.rb

package com.trafigura.edm.tradeservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import org.jboss.resteasy.client._

import com.trafigura.edm.trades._
import com.trafigura.edm.tradeservice._
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.edm.shared.types._


/**
 * Locator for finding EdmGetRealTrades instances.
 */
trait EdmGetRealTradesLocator {
  /**
   * Provides a proxy for the given operation to run.
   */
  def proxy:EdmGetRealTrades
}

class LocatableEdmGetRealTrades(val locator:EdmGetRealTradesLocator) extends EdmGetRealTrades {
  
    def getAll(): com.trafigura.edm.tradeservice.TradeResults =
      locator.proxy.getAll()
  
    def get(identifier: com.trafigura.edm.shared.types.TitanId): com.trafigura.edm.trades.Trade =
      locator.proxy.get(identifier)
  
    def getVersions(identifier: com.trafigura.edm.shared.types.TitanId): List[com.trafigura.edm.trades.Trade] =
      locator.proxy.getVersions(identifier)
  
    def getVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime): com.trafigura.edm.trades.Trade =
      locator.proxy.getVersion(identifier, asOf)
  
    def getByGuid(guid: GUID): com.trafigura.edm.trades.Trade =
      locator.proxy.getByGuid(guid)
  
    def getQuota(identifier: com.trafigura.edm.shared.types.TitanId): com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota =
      locator.proxy.getQuota(identifier)
  
    def getQuotaVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime): com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota =
      locator.proxy.getQuotaVersion(identifier, asOf)
  
}