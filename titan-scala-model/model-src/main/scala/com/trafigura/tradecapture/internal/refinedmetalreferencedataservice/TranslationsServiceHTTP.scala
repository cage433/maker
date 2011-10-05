/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_service_http.rb

package com.trafigura.tradecapture.internal.refinedmetalreferencedataservice

import com.trafigura.tradecapture.internal.refinedmetal._

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


// JAX-RS support for TranslationsService


// A resource trait corresponding to TranslationsService

@Path("/TranslationsService")
trait TranslationsServiceResource {
  
  @Path("GetAllTranslations")
  @POST  @Produces(Array("application/json"))
  def getAllTranslations(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetTranslationsForLanguage")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getTranslationsForLanguage(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("SaveTranslations")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def saveTranslations(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("DeleteTranslations")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteTranslations(@HeaderParam("User") _userHeader:String, in: String): Response
  
  @Path("GetAllLanguages")
  @POST  @Produces(Array("application/json"))
  def getAllLanguages(@HeaderParam("User") _userHeader:String): Response
  
  @Path("GetTranslations")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def getTranslations(@HeaderParam("User") _userHeader:String, in: String): Response
  
}


// Wrap a TranslationsServiceResource (provided by RESTEasy's client
// support) as a TranslationsService
//
// Note: will throw an exception on parse/status code errors, exception will contain details of the error
//
class TranslationsServiceResourceProxy(val resource: TranslationsServiceResource) extends TranslationsService {

  val log = org.slf4j.LoggerFactory.getLogger(this.getClass.getName)

  
  def getAllTranslations() = getAllTranslations(null)
  def getAllTranslations( securityContext:SecurityContext = null): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getAllTranslations(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getAllTranslations: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Translation.fromJson(o, cache))) })
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
  
  def getTranslationsForLanguage(languageCode: String) = getTranslationsForLanguage(languageCode,null)
  def getTranslationsForLanguage(languageCode: String, securityContext:SecurityContext = null): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getTranslationsForLanguage(_userHeader, TranslationsServiceJSON.serializeGetTranslationsForLanguage(languageCode).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getTranslationsForLanguage: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Translation.fromJson(o, cache))) })
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
  
  def saveTranslations(translations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation]) = saveTranslations(translations,null)
  def saveTranslations(translations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation], securityContext:SecurityContext = null): List[com.trafigura.tradecapture.internal.refinedmetal.Translation] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.saveTranslations(_userHeader, TranslationsServiceJSON.serializeSaveTranslations(translations).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to saveTranslations: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Translation.fromJson(o, cache))) })
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
  
  def deleteTranslations(translations: List[Int]) = deleteTranslations(translations,null)
  def deleteTranslations(translations: List[Int], securityContext:SecurityContext = null): Boolean = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.deleteTranslations(_userHeader, TranslationsServiceJSON.serializeDeleteTranslations(translations).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to deleteTranslations: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optional[Boolean](obj, false)
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
  
  def getAllLanguages() = getAllLanguages(null)
  def getAllLanguages( securityContext:SecurityContext = null): List[com.trafigura.tradecapture.internal.refinedmetal.Language] = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getAllLanguages(_userHeader)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getAllLanguages: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          JSONConversions.optionalList(obj, x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Language.fromJson(o, cache))) })
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
  
  def getTranslations(languageCode: String, regTranslations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation]) = getTranslations(languageCode, regTranslations,null)
  def getTranslations(languageCode: String, regTranslations: List[com.trafigura.tradecapture.internal.refinedmetal.Translation], securityContext:SecurityContext = null): com.trafigura.tradecapture.internal.refinedmetal.Translations = {
    val _userHeader = if (securityContext != null) { securityContext.user } else { null }
    val response = resource.getTranslations(_userHeader, TranslationsServiceJSON.serializeGetTranslations(languageCode, regTranslations).toString)
    val content = response.asInstanceOf[ClientResponse[String]].getEntity(classOf[String])

    log.debug("Completed call to getTranslations: " + response.getStatus() + " - " + content)

    if (response.getStatus() == Response.Status.OK.getStatusCode()) {
      
	      try {
          val obj = JSONConversions.parseJSON(content)
          val cache = new DeserialisationHelper
          (obj) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Translations.fromJson(o, cache))) }
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

// Wrap a TranslationsService as a TranslationsServiceResource (e.g. to be
// exposed by a JAX-RS implementation).

class TranslationsServiceResourceStub(val target: TranslationsService, filters: java.util.List[ServiceFilter])
    extends AbstractResourceStub(filters)
    with TranslationsServiceResource {

  def this(target: TranslationsService) = this(target, new java.util.ArrayList[ServiceFilter])

  requireFilters("com.trafigura.tradinghub.support.PermissionFilter","com.trafigura.services.security.ServiceOperationFilterBase")

  
  def getAllTranslations(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "GetAllTranslations", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getTranslationsForLanguage(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "GetTranslationsForLanguage", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def saveTranslations(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "SaveTranslations", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def deleteTranslations(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "DeleteTranslations", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getAllLanguages(_userHeader:String): Response = {

    try {
    val params = new JSONObject()
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "GetAllLanguages", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
  def getTranslations(_userHeader:String, in: String): Response = {

    try {
    val params = new JSONObject(in)
     val res = 
      TranslationsServiceJSON.dispatch(target, filters.toSeq, "GetTranslations", params, buildSecurityContext(_userHeader))

    
    Response.ok(JSONConversions.stringifyJSON(res.opt("result"))).build
    
    } catch {
      case ex:Throwable => generateErrorEntity(ex)
    }
  }
  
}
