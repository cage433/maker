/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Currency extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var oid:Int = 0
    
    
    

var guid:GUID = null
    
    
    

var expired:Boolean = false
    
    
    

var name:String = null
    
    
    

var mappingCode:String = null
    
    
    

var refDataBaseId:Int = 0
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Currency]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Currency]
            that.canEqual(this) && this.expired == that.expired && this.guid == that.guid && this.mappingCode == that.mappingCode && this.name == that.name && this.oid == that.oid && this.refDataBaseId == that.refDataBaseId &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Currency]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         expired.hashCode
        
    ) +
         (if (guid == null) 0 else guid.hashCode) 
    ) +
         (if (mappingCode == null) 0 else mappingCode.hashCode) 
    ) +
         (if (name == null) 0 else name.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         refDataBaseId.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.Currency: " + "" + "expired = " + expired + ", " + "guid = " + guid + ", " + "mappingCode = " + mappingCode + ", " + "name = " + name + ", " + "oid = " + oid + ", " + "refDataBaseId = " + refDataBaseId +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.Currency@1@1.0")
        
          
          
          
          
            result.putOpt("Oid", this.oid);
          
        
          
          
          
          
            result.putOpt("Guid", if (this.guid == null) null else this.guid.toJson);
          
        
          
          
          
          
            result.putOpt("Expired", this.expired);
          
        
          
          
          
          
            result.putOpt("Name", this.name);
          
        
          
          
          
          
            result.putOpt("MappingCode", this.mappingCode);
          
        
          
          
          
          
            result.putOpt("RefDataBaseId", this.refDataBaseId);
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              oid = JSONConversions.optional[Int](jobj.opt("Oid"), 0)
          
              guid = if (jobj.opt("Guid") == null) null else GUID(JSONConversions.optional[String](jobj.opt("Guid"), null))
          
              expired = JSONConversions.optional[Boolean](jobj.opt("Expired"), false)
          
              name = JSONConversions.optional[String](jobj.opt("Name"), null)
          
              mappingCode = JSONConversions.optional[String](jobj.opt("MappingCode"), null)
          
              refDataBaseId = jobj.optInt("RefDataBaseId")
          
        
    }

  
}

object Currency {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.Currency"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.Currency = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.Currency")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.Currency")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.Currency()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  
  val AED = "AED"
  
  val ECB = "ECB"
  
  val EUR = "EUR"
  
  val FX1 = "FX1"
  
  val GBP = "GBP"
  
  val ITL = "ITL"
  
  val JPY = "JPY"
  
  val RMB = "RMB"
  
  val USD = "USD"
  
  val ZAR = "ZAR"
  
  val Aed = 1
  
  val Ecb = 2
  
  val Eur = 3
  
  val Fx1 = 4
  
  val Gbp = 5
  
  val Itl = 6
  
  val Jpy = 7
  
  val Rmb = 8
  
  val Usd = 9
  
  val Zar = 10
  

  
    def apply(oid: Int = 0, guid: GUID = null, expired: Boolean = false, name: String = null, mappingCode: String = null, refDataBaseId: Int = 0) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.Currency
      
        res.oid = oid
      
        res.guid = guid
      
        res.expired = expired
      
        res.name = name
      
        res.mappingCode = mappingCode
      
        res.refDataBaseId = refDataBaseId
      
      res
    }
  
}
