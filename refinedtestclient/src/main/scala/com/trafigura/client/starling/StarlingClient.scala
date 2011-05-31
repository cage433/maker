package com.trafigura.client.starling

import org.springframework.context.support.ClassPathXmlApplicationContext
import com.trafigura.edm.tradeservice.{EdmGetTrades, EdmGetTradesResource, EdmGetTradesResourceProxy}
import com.trafigura.starling.{StarlingResourceProxy, StarlingResource}
import org.jboss.resteasy.client.ProxyFactory
import com.trafigura.services.security._

object StarlingClient {
  def main(args:Array[String]) = {
    val context = new ClassPathXmlApplicationContext("starling-client.xml")
    val clientExecutor = context.getBean("clientExecutor").asInstanceOf[ClientUserExecutor]
    def tradeServiceURL = context.getBean("tradeServiceRPC").asInstanceOf[String]

//    val edmGetTradesProxy = new EdmGetTradesResourceProxy(ProxyFactory.create(classOf[EdmGetTradesResource], tradeServiceURL, clientExecutor))
//    val trades = edmGetTradesProxy.getAll()
//    trades.foreach(t => println(t.toJson.toString(4)))

    val starlingProxy = new StarlingResourceProxy(ProxyFactory.create(classOf[StarlingResource], 
     "http://localhost:8080/starling/RPC", 
     //"http://localhost:8080/tradeservice/RCP",
      clientExecutor))

    println("\n\nCalling \"println(starlingProxy.helloWorld(\"Tim\", \"Cain\")), produces\n")

    //println("Hello")


    println(starlingProxy.helloWorld("Tim", "Cain"))
  }
}
