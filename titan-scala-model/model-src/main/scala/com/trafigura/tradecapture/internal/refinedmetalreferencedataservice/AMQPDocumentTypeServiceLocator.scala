/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator_amqp.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import com.trafigura.tradinghub.services.discovery.ServiceAdvertisement
import org.jboss.resteasy.client._

class AMQPDocumentTypeServiceLocator(ch:ConnectorHolder) extends AbstractServiceLocator[DocumentTypeService](ch) with DocumentTypeServiceLocator {
  def serviceKey = "DocumentTypeService"

  def createProxy(advertisement:ServiceAdvertisement):DocumentTypeService =
    new DocumentTypeServiceResourceProxy(ProxyFactory.create(classOf[DocumentTypeServiceResource], advertisement.address))
}

class AMQPLocatableDocumentTypeService(ch:ConnectorHolder)
  extends LocatableDocumentTypeService(new AMQPDocumentTypeServiceLocator(ch))