/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator_amqp.rb

package com.trafigura.edm.logistics.inventory

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import com.trafigura.tradinghub.services.discovery.ServiceAdvertisement
import org.jboss.resteasy.client._

class AMQPEdmAssignmentServiceLocator(ch:ConnectorHolder) extends AbstractServiceLocator[EdmAssignmentService](ch) with EdmAssignmentServiceLocator {
  def serviceKey = "EdmAssignmentService"

  def createProxy(advertisement:ServiceAdvertisement):EdmAssignmentService =
    new EdmAssignmentServiceResourceProxy(ProxyFactory.create(classOf[EdmAssignmentServiceResource], advertisement.address))
}

class AMQPLocatableEdmAssignmentService(ch:ConnectorHolder)
  extends LocatableEdmAssignmentService(new AMQPEdmAssignmentServiceLocator(ch))