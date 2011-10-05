/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_locator_amqp.rb

package com.trafigura.tradecapture.internal.permissionservice

import com.trafigura.tradinghub.support._
import com.trafigura.tradinghub.discovery._
import com.trafigura.tradinghub.services.discovery.ServiceAdvertisement
import org.jboss.resteasy.client._

class AMQPUserServiceLocator(ch:ConnectorHolder) extends AbstractServiceLocator[UserService](ch) with UserServiceLocator {
  def serviceKey = "UserService"

  def createProxy(advertisement:ServiceAdvertisement):UserService =
    new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], advertisement.address))
}

class AMQPLocatableUserService(ch:ConnectorHolder)
  extends LocatableUserService(new AMQPUserServiceLocator(ch))