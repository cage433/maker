/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.fpc





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._



/**
 Represents a FX based hedge request for a specified amount
 */


 class FxHedgeRequest extends com.trafigura.edm.fpc.HedgeRequest  {
  


  
    

var amount:com.trafigura.edm.shared.types.EDMQuantity = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.fpc.FxHedgeRequest]) {
            val that = r.asInstanceOf[com.trafigura.edm.fpc.FxHedgeRequest]
            that.canEqual(this) && this.amount == that.amount && this.contractMonthName == that.contractMonthName && this.directionId == that.directionId && this.exchangeId == that.exchangeId && this.hedgePrice == that.hedgePrice && this.hedgeRequestId == that.hedgeRequestId && this.oid == that.oid && this.ordinal == that.ordinal &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.fpc.FxHedgeRequest]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (amount == null) 0 else amount.hashCode) 
    ) +
         (if (contractMonthName == null) 0 else contractMonthName.hashCode) 
    ) +
         directionId.hashCode
        
    ) +
         exchangeId.hashCode
        
    ) +
         (if (hedgePrice == null) 0 else hedgePrice.hashCode) 
    ) +
         (if (hedgeRequestId == null) 0 else hedgeRequestId.hashCode) 
    ) +
         oid.hashCode
        
    ) +
         ordinal.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.edm.fpc.FxHedgeRequest: " + "" + "amount = " + amount + ", " + "contractMonthName = " + contractMonthName + ", " + "directionId = " + directionId + ", " + "exchangeId = " + exchangeId + ", " + "hedgePrice = " + hedgePrice + ", " + "hedgeRequestId = " + hedgeRequestId + ", " + "oid = " + oid + ", " + "ordinal = " + ordinal +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.FPC.FxHedgeRequest@1@1.0")
        
          
          
          
          
            result.putOpt("Amount", (this.amount) match { case null => null; case o => o.toJson(differentiator) });
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              amount = (jobj.opt("Amount")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.shared.types.EDMQuantity.fromJson(o, cache))) }
          
        
    }

  
}

object FxHedgeRequest {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.FPC.FxHedgeRequest"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.fpc.FxHedgeRequest = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.fpc.FxHedgeRequest")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.fpc.FxHedgeRequest")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.fpc.FxHedgeRequest()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(amount: com.trafigura.edm.shared.types.EDMQuantity = null) = {
      val res = new com.trafigura.edm.fpc.FxHedgeRequest
      
        res.amount = amount
      
      res
    }
  
}
