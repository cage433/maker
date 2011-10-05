/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetaltradeservice





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class FilteredTradeBlotterRowResults extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var filtered:Boolean = false
    
    
    

var tradeBlotterRow:com.trafigura.tradecapture.internal.refinedmetal.TradeBlotterRow = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults]
            that.canEqual(this) && this.filtered == that.filtered && this.tradeBlotterRow == that.tradeBlotterRow &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults]

  override def hashCode = {
    (41 * (41 *  1
    ) +
         filtered.hashCode
        
    ) +
         (if (tradeBlotterRow == null) 0 else tradeBlotterRow.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults: " + "" + "filtered = " + filtered + ", " + "tradeBlotterRow = " + tradeBlotterRow +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetalTradeService.FilteredTradeBlotterRowResults@1@1.0")
        
          
          
          
          
            result.putOpt("Filtered", this.filtered);
          
        
          
          
          
          
            result.putOpt("TradeBlotterRow", (this.tradeBlotterRow) match { case null => null; case o => o.toJson(differentiator) });
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              filtered = JSONConversions.optional[Boolean](jobj.opt("Filtered"), false)
          
              tradeBlotterRow = (jobj.opt("TradeBlotterRow")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.TradeBlotterRow.fromJson(o, cache))) }
          
        
    }

  
}

object FilteredTradeBlotterRowResults {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetalTradeService.FilteredTradeBlotterRowResults"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(filtered: Boolean = false, tradeBlotterRow: com.trafigura.tradecapture.internal.refinedmetal.TradeBlotterRow = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetaltradeservice.FilteredTradeBlotterRowResults
      
        res.filtered = filtered
      
        res.tradeBlotterRow = tradeBlotterRow
      
      res
    }
  
}
