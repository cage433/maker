/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_http.rb

package com.trafigura.tradecapture.internal.refinedmetaltradeservice

import com.trafigura.tradecapture.internal.refinedmetaltradeservice._

import org.codehaus.jettison.json.JSONObject
import com.trafigura.tradinghub.support._
import javax.ws.rs._
import javax.ws.rs.core.Response
import org.jboss.resteasy.client.ClientResponse
import org.jboss.resteasy.annotations._
import scala.collection.JavaConversions._

// Some of the objects from the system namespace responses are
// used in handwritten code, so this hybrid resolution is required.
import com.trafigura.responses._


// JAX-RS support for SyncTrades


// A resource trait corresponding to SyncTrades

@Path("/SyncTrades")
trait SyncTradesResource {
  
  @Path("SyncCompletedTradeWithNeptune")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def syncCompletedTradeWithNeptune(@HeaderParam("User") _userHeader:String, in: String): Response
  
}


// Wrap a SyncTradesResource (provided by RESTEasy's client
// support) as a SyncTrades
//
// Note: will throw an exception on parse/status code errors, exception will contain details of the error
//
class SyncTradesResourceProxy(val resource: SyncTradesResource) extends SyncTrades {

  val log = org.slf4j.LoggerFactory.getLogger(this.getClass.getName)

  
  def syncCompletedTradeWithNeptune(neptuneId: String) = syncCompletedTradeWithNeptune(neptuneId,null)
  def syncCompletedTradeWithNeptune(neptuneId: String, securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.SyncResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.syncCompletedTradeWithNeptune(_userHeader, SyncTradesJSON.serializeSyncCompletedTradeWithNeptune(neptuneId).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to syncCompletedTradeWithNeptune: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.SyncResult.fromJson(o, cache))) }
	      } catch {
          case ex : Exception => {
            log.error("Parse exception from Status-Code: " + response.getStatus() + " on content: " + content + "; throwing exception...", ex)
            throw new Exception("Service invocation, response parse exception: Status Code: " + response.getStatus() + ", Content: '" + content + "'")
          }
        }
      
    }
    else { // non OK 200 status response
      log.debug("status NOT OK 200, throwing exception...")

      val obj = JSONConversions.parseJSON(content)

      val err = (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => Result.fromJson(o, new DeserialisationHelper) }
        err match {
          case er : Result => {
            log.error("error response: " + er.message  + " { " + er.toString + " } status: " + response.getStatus)
            val ex = new Exception(er.message  + " { " + er.toString + " } status: " + response.getStatus)
            //ex.setStackTrace(er.stackTrace) // todo, need to add server stack trace, probably to customer application exception class
            throw ex
          }
          case _ => {
            log.error("unknown response, not an error response, content: " + content)
            throw new Exception("Unhandled response received, content: " + content)
          }
        }
    }
  }
  
}

// Wrap a SyncTrades as a SyncTradesResource (e.g. to be
// exposed by a JAX-RS implementation).

class SyncTradesResourceStub(val target: SyncTrades, filters: java.util.List[ServiceFilter])
    extends AbstractResourceStub(filters)
    with SyncTradesResource {

  def this(target: SyncTrades) = this(target, new java.util.ArrayList[ServiceFilter])

  requireFilters("com.trafigura.tradinghub.support.PermissionFilter","com.trafigura.services.security.ServiceOperationFilterBase")

  
  def syncCompletedTradeWithNeptune(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      SyncTradesJSON.dispatch(target, filters.toSeq, "SyncCompletedTradeWithNeptune", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
}
