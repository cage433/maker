/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_http.rb

package com.trafigura.edm.logistics.inventory

import com.trafigura.edm.logistics.inventory._

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


// JAX-RS support for EdmAssignmentService


// A resource trait corresponding to EdmAssignmentService

@Path("/EdmAssignmentService")
trait EdmAssignmentServiceResource {
  
  @Path("GetAllSalesAssignments")
  @POST  @Produces(Array("application/json"))
  def getAllSalesAssignments(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetAssignmentById")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getAssignmentById(@HeaderParam("User") _userHeader:String, in: String): Response
  
}


// Wrap a EdmAssignmentServiceResource (provided by RESTEasy's client
// support) as a EdmAssignmentService
//
// Note: will throw an exception on parse/status code errors, exception will contain details of the error
//
class EdmAssignmentServiceResourceProxy(val resource: EdmAssignmentServiceResource) extends EdmAssignmentService {

  val log = org.slf4j.LoggerFactory.getLogger(this.getClass.getName)

  
  def getAllSalesAssignments() = getAllSalesAssignments(null)
  def getAllSalesAssignments( securityContext:SecurityContext = null): List[com.trafigura.edm.logistics.inventory.EDMAssignment] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getAllSalesAssignments(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getAllSalesAssignments: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.logistics.inventory.EDMAssignment.fromJson(o, cache))) })
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
  
  def getAssignmentById(assignmentId: Int) = getAssignmentById(assignmentId,null)
  def getAssignmentById(assignmentId: Int, securityContext:SecurityContext = null): com.trafigura.edm.logistics.inventory.EDMAssignment = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getAssignmentById(_userHeader, EdmAssignmentServiceJSON.serializeGetAssignmentById(assignmentId).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getAssignmentById: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.logistics.inventory.EDMAssignment.fromJson(o, cache))) }
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

// Wrap a EdmAssignmentService as a EdmAssignmentServiceResource (e.g. to be
// exposed by a JAX-RS implementation).

class EdmAssignmentServiceResourceStub(val target: EdmAssignmentService, filters: java.util.List[ServiceFilter])
    extends AbstractResourceStub(filters)
    with EdmAssignmentServiceResource {

  def this(target: EdmAssignmentService) = this(target, new java.util.ArrayList[ServiceFilter])

  requireFilters("com.trafigura.tradinghub.support.PermissionFilter","com.trafigura.services.security.ServiceOperationFilterBase")

  
  def getAllSalesAssignments(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      EdmAssignmentServiceJSON.dispatch(target, filters.toSeq, "GetAllSalesAssignments", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getAssignmentById(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      EdmAssignmentServiceJSON.dispatch(target, filters.toSeq, "GetAssignmentById", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
}
