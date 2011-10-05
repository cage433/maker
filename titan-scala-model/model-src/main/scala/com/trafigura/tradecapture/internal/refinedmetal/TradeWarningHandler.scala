/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version .
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_handler.rb

package com.trafigura.tradecapture.internal.refinedmetal

import com.trafigura.tradinghub.support.{MessageResultListener, MessageProperties}

trait TradeWarningHandler {
  /**
   * Indicates to the handler that a set of com.trafigura.tradecapture.internal.refinedmetal.TradeWarning objects has arrived and is ready to be processed.
   * Upon completing processing the objects, either acknowledge or reject should be called on the resultListener,
   * allowing transactional transports to release the message in an appropriate manner.
   */
  def onTradeWarning(props:MessageProperties, objs:List[com.trafigura.tradecapture.internal.refinedmetal.TradeWarning], resultListener:MessageResultListener)
}
