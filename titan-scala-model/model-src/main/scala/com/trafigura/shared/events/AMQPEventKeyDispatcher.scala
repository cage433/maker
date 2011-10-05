/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version .
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_dispatcher_amqp.erb

package com.trafigura.shared.events

import com.trafigura.tradinghub.support._

/**
 * Dispatcher for sending com.trafigura.shared.events.EventKey entities.
 */
class AMQPEventKeyDispatcher(connectorHolder:ConnectorHolder, appName:String)
    extends AbstractAMQPDispatcher(connectorHolder)
    with EventKeyDispatcher {

  def exchangeName = "Trafigura.EventKey." + appName

  def dispatchEventKey(objs:List[com.trafigura.shared.events.EventKey], metadata:Map[String, String]) {
    val arr = new org.codehaus.jettison.json.JSONArray(java.util.Arrays.asList(objs.map(t => t.toJson()).toArray: _*))
    dispatch(arr.toString.getBytes("UTF-8"), "", metadata)
  }
}
