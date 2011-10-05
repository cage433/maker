/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetaltradeservice





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class NeptuneResult extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var success:Boolean = false
    
    
    

var quotaInfo:List[com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneQuotaInfo] = null
    
    
    

var neptuneId:String = null
    
    
    

var error:String = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult]
            that.canEqual(this) && this.error == that.error && this.neptuneId == that.neptuneId && this.quotaInfo == that.quotaInfo && this.success == that.success &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult]

  override def hashCode = {
    (41 * (41 * (41 * (41 *  1
    ) +
         (if (error == null) 0 else error.hashCode) 
    ) +
         (if (neptuneId == null) 0 else neptuneId.hashCode) 
    ) +
         (if (quotaInfo == null) 0 else quotaInfo.hashCode) 
    ) +
         success.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult: " + "" + "error = " + error + ", " + "neptuneId = " + neptuneId + ", " + "quotaInfo = " + quotaInfo + ", " + "success = " + success +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetalTradeService.NeptuneResult@1@1.0")
        
          
          
          
          
            result.putOpt("Success", this.success);
          
        
          
          
          
          
            result.putOpt("QuotaInfo", new org.codehaus.jettison.json.JSONArray(if(this.quotaInfo == null) new java.util.ArrayList() else java.util.Arrays.asList(this.quotaInfo.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("NeptuneId", this.neptuneId);
          
        
          
          
          
          
            result.putOpt("Error", this.error);
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              success = JSONConversions.optional[Boolean](jobj.opt("Success"), false)
          
              quotaInfo = JSONConversions.optionalList(jobj.opt("QuotaInfo"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneQuotaInfo.fromJson(o, cache))) })
          
              neptuneId = JSONConversions.optional[String](jobj.opt("NeptuneId"), null)
          
              error = JSONConversions.optional[String](jobj.opt("Error"), null)
          
        
    }

  
}

object NeptuneResult {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetalTradeService.NeptuneResult"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(success: Boolean = false, quotaInfo: List[com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneQuotaInfo] = null, neptuneId: String = null, error: String = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetaltradeservice.NeptuneResult
      
        res.success = success
      
        res.quotaInfo = quotaInfo
      
        res.neptuneId = neptuneId
      
        res.error = error
      
      res
    }
  
}
