/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.fpc





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class UnkPricingSpec extends com.trafigura.edm.fpc.PricingSpec  {
  


  
    

var marketId:Int = 0
    
    
    

var qpMonth:org.joda.time.LocalDate = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.fpc.UnkPricingSpec]) {
            val that = r.asInstanceOf[com.trafigura.edm.fpc.UnkPricingSpec]
            that.canEqual(this) && this.comments == that.comments && this.currencyId == that.currencyId && this.hedgeRequests == that.hedgeRequests && this.marketId == that.marketId && this.neptuneHedgeRequests == that.neptuneHedgeRequests && this.neptunePricingFixations == that.neptunePricingFixations && this.oid == that.oid && this.ordinal == that.ordinal && this.premium == that.premium && this.premiumComments == that.premiumComments && this.pricingFixations == that.pricingFixations && this.qpMonth == that.qpMonth && this.quantity == that.quantity &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.fpc.UnkPricingSpec]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (comments == null) 0 else comments.hashCode) 
    ) +
         currencyId.hashCode
        
    ) +
         (if (hedgeRequests == null) 0 else hedgeRequests.hashCode) 
    ) +
         marketId.hashCode
        
    ) +
         (if (neptuneHedgeRequests == null) 0 else neptuneHedgeRequests.hashCode) 
    ) +
         (if (neptunePricingFixations == null) 0 else neptunePricingFixations.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         ordinal.hashCode
        
    ) +
         (if (premium == null) 0 else premium.hashCode) 
    ) +
         (if (premiumComments == null) 0 else premiumComments.hashCode) 
    ) +
         (if (pricingFixations == null) 0 else pricingFixations.hashCode) 
    ) +
         (if (qpMonth == null) 0 else qpMonth.hashCode) 
    ) +
         (if (quantity == null) 0 else quantity.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.fpc.UnkPricingSpec: " + "" + "comments = " + comments + ", " + "currencyId = " + currencyId + ", " + "hedgeRequests = " + hedgeRequests + ", " + "marketId = " + marketId + ", " + "neptuneHedgeRequests = " + neptuneHedgeRequests + ", " + "neptunePricingFixations = " + neptunePricingFixations + ", " + "oid = " + oid + ", " + "ordinal = " + ordinal + ", " + "premium = " + premium + ", " + "premiumComments = " + premiumComments + ", " + "pricingFixations = " + pricingFixations + ", " + "qpMonth = " + qpMonth + ", " + "quantity = " + quantity +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.FPC.UnkPricingSpec@1@1.0")
        
          
          
          
          
            result.putOpt("MarketId", this.marketId);
          
        
          
          
          
          
            result.putOpt("QpMonth", JSONConversions.optionalDateToJSON(this.qpMonth));
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              marketId = jobj.optInt("MarketId")
          
              qpMonth = JSONConversions.optionalDate(jobj.opt("QpMonth"))
          
        
    }

  
}

object UnkPricingSpec {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.FPC.UnkPricingSpec"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.fpc.UnkPricingSpec = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.fpc.UnkPricingSpec")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.fpc.UnkPricingSpec")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.fpc.UnkPricingSpec()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(marketId: Int = 0, qpMonth: org.joda.time.LocalDate = null) = {
      val res = new com.trafigura.edm.fpc.UnkPricingSpec
      
        res.marketId = marketId
      
        res.qpMonth = qpMonth
      
      res
    }
  
}
