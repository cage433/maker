/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Shape extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var oid:Int = 0
    
    
    

var guid:GUID = null
    
    
    

var expired:Boolean = false
    
    
    

var spec:String = null
    
    
    

var metal:Int = 0
    
    
    

var name:String = null
    
    
    

var code:String = null
    
    
    

var mappingCode:String = null
    
    
    

var refDataBaseId:Int = 0
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Shape]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Shape]
            that.canEqual(this) && this.code == that.code && this.expired == that.expired && this.guid == that.guid && this.mappingCode == that.mappingCode && this.metal == that.metal && this.name == that.name && this.oid == that.oid && this.refDataBaseId == that.refDataBaseId && this.spec == that.spec &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.Shape]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (code == null) 0 else code.hashCode) 
    ) +
         expired.hashCode
        
    ) +
         (if (guid == null) 0 else guid.hashCode) 
    ) +
         (if (mappingCode == null) 0 else mappingCode.hashCode) 
    ) +
         metal.hashCode
        
    ) +
         (if (name == null) 0 else name.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         refDataBaseId.hashCode
        
    ) +
         (if (spec == null) 0 else spec.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.Shape: " + "" + "code = " + code + ", " + "expired = " + expired + ", " + "guid = " + guid + ", " + "mappingCode = " + mappingCode + ", " + "metal = " + metal + ", " + "name = " + name + ", " + "oid = " + oid + ", " + "refDataBaseId = " + refDataBaseId + ", " + "spec = " + spec +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.Shape@1@1.0")
        
          
          
          
          
            result.putOpt("Oid", this.oid);
          
        
          
          
          
          
            result.putOpt("Guid", if (this.guid == null) null else this.guid.toJson);
          
        
          
          
          
          
            result.putOpt("Expired", this.expired);
          
        
          
          
          
          
            result.putOpt("Spec", this.spec);
          
        
          
          
          
          
            result.putOpt("Metal", this.metal);
          
        
          
          
          
          
            result.putOpt("Name", this.name);
          
        
          
          
          
          
            result.putOpt("Code", this.code);
          
        
          
          
          
          
            result.putOpt("MappingCode", this.mappingCode);
          
        
          
          
          
          
            result.putOpt("RefDataBaseId", this.refDataBaseId);
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              oid = JSONConversions.optional[Int](jobj.opt("Oid"), 0)
          
              guid = if (jobj.opt("Guid") == null) null else GUID(JSONConversions.optional[String](jobj.opt("Guid"), null))
          
              expired = JSONConversions.optional[Boolean](jobj.opt("Expired"), false)
          
              spec = JSONConversions.optional[String](jobj.opt("Spec"), null)
          
              metal = JSONConversions.optional[Int](jobj.opt("Metal"), 0)
          
              name = JSONConversions.optional[String](jobj.opt("Name"), null)
          
              code = JSONConversions.optional[String](jobj.opt("Code"), null)
          
              mappingCode = JSONConversions.optional[String](jobj.opt("MappingCode"), null)
          
              refDataBaseId = jobj.optInt("RefDataBaseId")
          
        
    }

  
}

object Shape {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.Shape"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.Shape = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.Shape")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.Shape")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.Shape()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(oid: Int = 0, guid: GUID = null, expired: Boolean = false, spec: String = null, metal: Int = 0, name: String = null, code: String = null, mappingCode: String = null, refDataBaseId: Int = 0) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.Shape
      
        res.oid = oid
      
        res.guid = guid
      
        res.expired = expired
      
        res.spec = spec
      
        res.metal = metal
      
        res.name = name
      
        res.code = code
      
        res.mappingCode = mappingCode
      
        res.refDataBaseId = refDataBaseId
      
      res
    }
  
}
