/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_http.rb

package com.trafigura.edm.tradeservice

import com.trafigura.edm.trades._
import com.trafigura.edm.tradeservice._
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.edm.shared.types._

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


// JAX-RS support for EdmGetRealTrades


// A resource trait corresponding to EdmGetRealTrades

@Path("/EdmGetRealTrades")
trait EdmGetRealTradesResource {
  
  @Path("GetAll")
  @POST  @Produces(Array("application/json"))
  def getAll(@HeaderParam("User") _userHeader:String): Response
  
  @Path("Get")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def get(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetVersions")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getVersions(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetVersion")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getVersion(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetByGuid")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getByGuid(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetQuota")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getQuota(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetQuotaVersion")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getQuotaVersion(@HeaderParam("User") _userHeader:String, in: String): Response
  
}


// Wrap a EdmGetRealTradesResource (provided by RESTEasy's client
// support) as a EdmGetRealTrades
//
// Note: will throw an exception on parse/status code errors, exception will contain details of the error
//
class EdmGetRealTradesResourceProxy(val resource: EdmGetRealTradesResource) extends EdmGetRealTrades {

  val log = org.slf4j.LoggerFactory.getLogger(this.getClass.getName)

  
  def getAll() = getAll(null)
  def getAll( securityContext:SecurityContext = null): com.trafigura.edm.tradeservice.TradeResults = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getAll(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getAll: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.tradeservice.TradeResults.fromJson(o, cache))) }
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
  
  def get(identifier: com.trafigura.edm.shared.types.TitanId) = get(identifier,null)
  def get(identifier: com.trafigura.edm.shared.types.TitanId, securityContext:SecurityContext = null): com.trafigura.edm.trades.Trade = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.get(_userHeader, EdmGetRealTradesJSON.serializeGet(identifier).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to get: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.trades.Trade.fromJson(o, cache))) }
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
  
  def getVersions(identifier: com.trafigura.edm.shared.types.TitanId) = getVersions(identifier,null)
  def getVersions(identifier: com.trafigura.edm.shared.types.TitanId, securityContext:SecurityContext = null): List[com.trafigura.edm.trades.Trade] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getVersions(_userHeader, EdmGetRealTradesJSON.serializeGetVersions(identifier).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getVersions: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.trades.Trade.fromJson(o, cache))) })
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
  
  def getVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime) = getVersion(identifier, asOf,null)
  def getVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime, securityContext:SecurityContext = null): com.trafigura.edm.trades.Trade = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getVersion(_userHeader, EdmGetRealTradesJSON.serializeGetVersion(identifier, asOf).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getVersion: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.trades.Trade.fromJson(o, cache))) }
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
  
  def getByGuid(guid: GUID) = getByGuid(guid,null)
  def getByGuid(guid: GUID, securityContext:SecurityContext = null): com.trafigura.edm.trades.Trade = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getByGuid(_userHeader, EdmGetRealTradesJSON.serializeGetByGuid(guid).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getByGuid: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.trades.Trade.fromJson(o, cache))) }
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
  
  def getQuota(identifier: com.trafigura.edm.shared.types.TitanId) = getQuota(identifier,null)
  def getQuota(identifier: com.trafigura.edm.shared.types.TitanId, securityContext:SecurityContext = null): com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getQuota(_userHeader, EdmGetRealTradesJSON.serializeGetQuota(identifier).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getQuota: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota.fromJson(o, cache))) }
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
  
  def getQuotaVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime) = getQuotaVersion(identifier, asOf,null)
  def getQuotaVersion(identifier: com.trafigura.edm.shared.types.TitanId, asOf: org.joda.time.DateTime, securityContext:SecurityContext = null): com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getQuotaVersion(_userHeader, EdmGetRealTradesJSON.serializeGetQuotaVersion(identifier, asOf).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getQuotaVersion: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.physicaltradespecs.PhysicalTradeQuota.fromJson(o, cache))) }
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

// Wrap a EdmGetRealTrades as a EdmGetRealTradesResource (e.g. to be
// exposed by a JAX-RS implementation).

class EdmGetRealTradesResourceStub(val target: EdmGetRealTrades, filters: java.util.List[ServiceFilter])
    extends AbstractResourceStub(filters)
    with EdmGetRealTradesResource {

  def this(target: EdmGetRealTrades) = this(target, new java.util.ArrayList[ServiceFilter])

  requireFilters("com.trafigura.tradinghub.support.PermissionFilter","com.trafigura.services.security.ServiceOperationFilterBase")

  
  def getAll(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetAll", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def get(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "Get", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getVersions(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetVersions", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getVersion(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetVersion", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getByGuid(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetByGuid", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getQuota(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetQuota", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getQuotaVersion(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmGetRealTradesJSON.dispatch(target, filters.toSeq, "GetQuotaVersion", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
}
