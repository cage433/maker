package com.trafigura.starling.rpc

import com.trafigura.starling.Starling
import org.springframework.context.support.ClassPathXmlApplicationContext
import org.jboss.resteasy.client.ProxyFactory
import com.trafigura.edm.tradeservice.{EdmGetTrades, EdmGetTradesResource, EdmGetTradesResourceProxy}
import com.trafigura.edm.trades._
import com.trafigura.starling.{StarlingResourceProxy, StarlingResource}
import com.trafigura.services.security._
import starling.utils.MathUtil._
import com.trafigura.tradecapture.internal.refinedmetalreferencedataservice._
import starling.services.StarlingInit
import starling.props.Props

class StarlingRPC (clientExecutor: ClientUserExecutor, tradeServiceURL: String) extends Starling {


  def println(text : AnyRef) {
    System.err.println(text)
  }
  def helloWorld(string1: String, string2:String): String = {
//
//    val edmGetTradesProxy = new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, clientExecutor))
//
//    val trades = edmGetTradesProxy.getAll()
//    val quotas = trades.flatMap{
//      _ match {
//        case pt : PhysicalTrade => pt.quotas
//        case _ => Nil
//      }
//    }
//
//    println("Trades " + trades.size)
    //quotas.foreach{x => println(x)}

    
    "finished"
  }
}

trait PricingSpec{
//  def mtm (env : Environment) : Quantity
}

//case class MonthAveragePricingSpec(market : String, monthOffset : Int, premium : Quantity) extends PricingSpec
//case class FixedPriceComponent(date : Day, quantity : Quantity, price : Quantity)
//case class FixedPricingSpec(components : List[FixedPriceComponent]) extends PricingSpec

object StarlingRPC extends Application {
    val context = new ClassPathXmlApplicationContext("starling-client.xml")
    val clientExecutor = context.getBean("clientExecutor").asInstanceOf[ClientUserExecutor]
    def tradeServiceURL = context.getBean("tradeServiceRPC").asInstanceOf[String]
    new StarlingRPC(clientExecutor, tradeServiceURL).helloWorld("Alex", "McGuire")
}

