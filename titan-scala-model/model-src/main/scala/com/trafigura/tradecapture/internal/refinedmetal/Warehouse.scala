/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Warehouse extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var oid:Int = 0
    
    
    

var guid:GUID = null
    
    
    

var name:String = null
    
    
    

var expired:Boolean = false
    
    
    

var refDataBaseId:Int = 0
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Warehouse]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Warehouse]
            that.canEqual(this) && this.expired == that.expired && this.guid == that.guid && this.name == that.name && this.oid == that.oid && this.refDataBaseId == that.refDataBaseId &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Warehouse]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 *  1
    ) +
         expired.hashCode
        
    ) +
         (if (guid == null) 0 else guid.hashCode) 
    ) +
         (if (name == null) 0 else name.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         refDataBaseId.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.Warehouse: " + "" + "expired = " + expired + ", " + "guid = " + guid + ", " + "name = " + name + ", " + "oid = " + oid + ", " + "refDataBaseId = " + refDataBaseId +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.Warehouse@1@1.0")
        
          
          
          
          
            result.putOpt("Oid", this.oid);
          
        
          
          
          
          
            result.putOpt("Guid", if (this.guid == null) null else this.guid.toJson);
          
        
          
          
          
          
            result.putOpt("Name", this.name);
          
        
          
          
          
          
            result.putOpt("Expired", this.expired);
          
        
          
          
          
          
            result.putOpt("RefDataBaseId", this.refDataBaseId);
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              oid = JSONConversions.optional[Int](jobj.opt("Oid"), 0)
          
              guid = if (jobj.opt("Guid") == null) null else GUID(JSONConversions.optional[String](jobj.opt("Guid"), null))
          
              name = JSONConversions.optional[String](jobj.opt("Name"), null)
          
              expired = JSONConversions.optional[Boolean](jobj.opt("Expired"), false)
          
              refDataBaseId = jobj.optInt("RefDataBaseId")
          
        
    }

  
}

object Warehouse {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.Warehouse"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.Warehouse = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.Warehouse")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.Warehouse")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.Warehouse()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(oid: Int = 0, guid: GUID = null, name: String = null, expired: Boolean = false, refDataBaseId: Int = 0) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.Warehouse
      
        res.oid = oid
      
        res.guid = guid
      
        res.name = name
      
        res.expired = expired
      
        res.refDataBaseId = refDataBaseId
      
      res
    }
  
}
