/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_http.rb

package com.trafigura.tradecapture.internal.refinedmetaltradeservice

import com.trafigura.tradecapture.internal.refinedmetal._
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


// JAX-RS support for CaptureNeptuneTrades


// A resource trait corresponding to CaptureNeptuneTrades

@Path("/CaptureNeptuneTrades")
trait CaptureNeptuneTradesResource {
  
  @Path("Create")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def create(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("Destroy")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def destroy(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("Complete")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def complete(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("ApplyPriceFixations")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def applyPriceFixations(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("Login")
  @POST  @Produces(Array("application/json"))
  def login(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetVersion")
  @POST  @Produces(Array("application/json"))
  def getVersion(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetBuildConfiguration")
  @POST  @Produces(Array("application/json"))
  def getBuildConfiguration(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetProcessId")
  @POST  @Produces(Array("application/json"))
  def getProcessId(@HeaderParam("User") _userHeader:String): Response
  
}


// Wrap a CaptureNeptuneTradesResource (provided by RESTEasy's client
// support) as a CaptureNeptuneTrades
//
// Note: will throw an exception on parse/status code errors, exception will contain details of the error
//
class CaptureNeptuneTradesResourceProxy(val resource: CaptureNeptuneTradesResource) extends CaptureNeptuneTrades {

  val log = org.slf4j.LoggerFactory.getLogger(this.getClass.getName)

  
  def create(trade: com.trafigura.tradecapture.internal.refinedmetal.RefinedMetalTrade) = create(trade,null)
  def create(trade: com.trafigura.tradecapture.internal.refinedmetal.RefinedMetalTrade, securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.create(_userHeader, CaptureNeptuneTradesJSON.serializeCreate(trade).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to create: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult.fromJson(o, cache))) }
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
  
  def destroy(neptuneId: String, groupCompanyId: Int) = destroy(neptuneId, groupCompanyId,null)
  def destroy(neptuneId: String, groupCompanyId: Int, securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.destroy(_userHeader, CaptureNeptuneTradesJSON.serializeDestroy(neptuneId, groupCompanyId).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to destroy: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult.fromJson(o, cache))) }
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
  
  def complete(neptuneId: String) = complete(neptuneId,null)
  def complete(neptuneId: String, securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.complete(_userHeader, CaptureNeptuneTradesJSON.serializeComplete(neptuneId).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to complete: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult.fromJson(o, cache))) }
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
  
  def applyPriceFixations(trade: com.trafigura.tradecapture.internal.refinedmetal.RefinedMetalTrade) = applyPriceFixations(trade,null)
  def applyPriceFixations(trade: com.trafigura.tradecapture.internal.refinedmetal.RefinedMetalTrade, securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.applyPriceFixations(_userHeader, CaptureNeptuneTradesJSON.serializeApplyPriceFixations(trade).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to applyPriceFixations: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult.fromJson(o, cache))) }
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
  
  def login() = login(null)
  def login( securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneLoginResult = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.login(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to login: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneLoginResult.fromJson(o, cache))) }
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
  
  def getVersion() = getVersion(null)
  def getVersion( securityContext:SecurityContext = null): String = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getVersion(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getVersion: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optional[String](obj, null)
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
  
  def getBuildConfiguration() = getBuildConfiguration(null)
  def getBuildConfiguration( securityContext:SecurityContext = null): String = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getBuildConfiguration(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getBuildConfiguration: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optional[String](obj, null)
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
  
  def getProcessId() = getProcessId(null)
  def getProcessId( securityContext:SecurityContext = null): Int = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getProcessId(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getProcessId: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optional[Int](obj, 0)
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

// Wrap a CaptureNeptuneTrades as a CaptureNeptuneTradesResource (e.g. to be
// exposed by a JAX-RS implementation).

class CaptureNeptuneTradesResourceStub(val target: CaptureNeptuneTrades, filters: java.util.List[ServiceFilter])
    extends AbstractResourceStub(filters)
    with CaptureNeptuneTradesResource {

  def this(target: CaptureNeptuneTrades) = this(target, new java.util.ArrayList[ServiceFilter])

  requireFilters("com.trafigura.tradinghub.support.PermissionFilter","com.trafigura.services.security.ServiceOperationFilterBase")

  
  def create(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "Create", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def destroy(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "Destroy", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def complete(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "Complete", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def applyPriceFixations(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "ApplyPriceFixations", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def login(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "Login", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getVersion(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "GetVersion", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getBuildConfiguration(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "GetBuildConfiguration", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getProcessId(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      CaptureNeptuneTradesJSON.dispatch(target, filters.toSeq, "GetProcessId", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
}
