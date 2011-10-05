/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version .
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_dispatcher.rb

package com.trafigura.exceptions

/**
 * Dispatcher that will send messages of the given type via the messaging transport.
 */
trait ProcessingExceptionDispatcher {
  /**
   * Dispatches a single com.trafigura.exceptions.ProcessingException.
   */
  def dispatchProcessingException(obj:com.trafigura.exceptions.ProcessingException):Unit = dispatchProcessingException(List(obj), Map[String, String]())

  /**
   * Dispatches a single com.trafigura.exceptions.ProcessingException.
   */
  def dispatchProcessingException(obj:com.trafigura.exceptions.ProcessingException, metadata:Map[String, String]):Unit =
    dispatchProcessingException(List(obj), metadata)

  /**
   * Dispatches a list of com.trafigura.exceptions.ProcessingException entities. These entities will be sent as a single
   * batch, and will be received by a corresponding receiver in the same grouping.
   */
  def dispatchProcessingException(objs:List[com.trafigura.exceptions.ProcessingException]):Unit =
    dispatchProcessingException(objs, Map[String, String]())

  /**
   * Dispatches a list of com.trafigura.exceptions.ProcessingException entities. These entities will be sent as a single
   * batch, and will be received by a corresponding receiver in the same grouping.
   */
  def dispatchProcessingException(objs:List[com.trafigura.exceptions.ProcessingException], metadata:Map[String, String]):Unit
}
