/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version .
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_dispatcher_amqp.erb

package com.trafigura.tradecapture.internal.refinedmetal

import com.trafigura.tradinghub.support._

/**
 * Dispatcher for sending com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters entities.
 */
class AMQPContractDocumentParametersDispatcher(connectorHolder:ConnectorHolder, appName:String)
    extends AbstractAMQPDispatcher(connectorHolder)
    with ContractDocumentParametersDispatcher {

  def exchangeName = "Trafigura.ContractDocumentParameters." + appName

  def dispatchContractDocumentParameters(objs:List[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters], metadata:Map[String, String]) {
    val arr = new org.codehaus.jettison.json.JSONArray(java.util.Arrays.asList(objs.map(t => t.toJson()).toArray: _*))
    dispatch(arr.toString.getBytes("UTF-8"), "", metadata)
  }
}
