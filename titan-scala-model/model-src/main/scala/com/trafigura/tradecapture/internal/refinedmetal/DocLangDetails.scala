/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class DocLangDetails extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var oid:Int = 0
    
    
    

var languageCode:String = null
    
    
    

var version:org.joda.time.DateTime = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails]
            that.canEqual(this) && this.languageCode == that.languageCode && this.oid == that.oid && this.version == that.version &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails]

  override def hashCode = {
    (41 * (41 * (41 *  1
    ) +
         (if (languageCode == null) 0 else languageCode.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         (if (version == null) 0 else version.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails: " + "" + "languageCode = " + languageCode + ", " + "oid = " + oid + ", " + "version = " + version +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.DocLangDetails@1@1.0")
        
          
          
          
          
            result.putOpt("Oid", this.oid);
          
        
          
          
          
          
            result.putOpt("LanguageCode", this.languageCode);
          
        
          
          
          
          
            result.putOpt("Version", JSONConversions.optionalDatetimeToJSON(this.version));
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              oid = JSONConversions.optional[Int](jobj.opt("Oid"), 0)
          
              languageCode = JSONConversions.optional[String](jobj.opt("LanguageCode"), null)
          
              version = JSONConversions.optionalDatetime(jobj.opt("Version"))
          
        
    }

  
}

object DocLangDetails {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.DocLangDetails"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(oid: Int = 0, languageCode: String = null, version: org.joda.time.DateTime = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.DocLangDetails
      
        res.oid = oid
      
        res.languageCode = languageCode
      
        res.version = version
      
      res
    }
  
}
