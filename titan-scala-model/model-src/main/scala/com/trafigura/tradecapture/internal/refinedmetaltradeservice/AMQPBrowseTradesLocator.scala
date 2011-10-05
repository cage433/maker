/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator_amqp.rb

package com.trafigura.tradecapture.internal.refinedmetaltradeservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import com.trafigura.tradinghub.services.discovery.ServiceAdvertisement
import org.jboss.resteasy.client._

class AMQPBrowseTradesLocator(ch:ConnectorHolder) extends AbstractServiceLocator[BrowseTrades](ch) with BrowseTradesLocator {
  def serviceKey = "BrowseTrades"

  def createProxy(advertisement:ServiceAdvertisement):BrowseTrades =
    new BrowseTradesResourceProxy(ProxyFactory.create(classOf[BrowseTradesResource], advertisement.address))
}

class AMQPLocatableBrowseTrades(ch:ConnectorHolder)
  extends LocatableBrowseTrades(new AMQPBrowseTradesLocator(ch))