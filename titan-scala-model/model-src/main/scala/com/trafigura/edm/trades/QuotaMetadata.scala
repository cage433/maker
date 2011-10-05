/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.trades





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._



/**
 used internally by the trade service to record metadata
// about the booking
 */


 class QuotaMetadata extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var quotaNumber:Int = 0
    
    
    

var neptuneFxPricing:Boolean = false
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.trades.QuotaMetadata]) {
            val that = r.asInstanceOf[com.trafigura.edm.trades.QuotaMetadata]
            that.canEqual(this) && this.neptuneFxPricing == that.neptuneFxPricing && this.quotaNumber == that.quotaNumber &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.trades.QuotaMetadata]

  override def hashCode = {
    (41 * (41 *  1
    ) +
         neptuneFxPricing.hashCode
        
    ) +
         quotaNumber.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.edm.trades.QuotaMetadata: " + "" + "neptuneFxPricing = " + neptuneFxPricing + ", " + "quotaNumber = " + quotaNumber +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.Trades.QuotaMetadata@1@1.0")
        
          
          
          
          
            result.putOpt("QuotaNumber", this.quotaNumber);
          
        
          
          
          
          
            result.putOpt("NeptuneFxPricing", this.neptuneFxPricing);
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              quotaNumber = JSONConversions.optional[Int](jobj.opt("QuotaNumber"), 0)
          
              neptuneFxPricing = JSONConversions.optional[Boolean](jobj.opt("NeptuneFxPricing"), false)
          
        
    }

  
}

object QuotaMetadata {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.Trades.QuotaMetadata"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.trades.QuotaMetadata = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.trades.QuotaMetadata")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.trades.QuotaMetadata")

      
        
            val result = new com.trafigura.edm.trades.QuotaMetadata()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(quotaNumber: Int = 0, neptuneFxPricing: Boolean = false) = {
      val res = new com.trafigura.edm.trades.QuotaMetadata
      
        res.quotaNumber = quotaNumber
      
        res.neptuneFxPricing = neptuneFxPricing
      
      res
    }
  
}
