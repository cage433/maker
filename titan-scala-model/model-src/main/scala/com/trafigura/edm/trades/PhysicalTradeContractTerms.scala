/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.trades





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class PhysicalTradeContractTerms extends com.trafigura.edm.trades.TradeContractTerms  {
  


  
    
    /**
     inspectionShareAtLoadPort should be the name and inspShareAtLP the label
     */

var inspShareAtLoadPort:com.trafigura.edm.shared.types.Percentage = null
    
    
    
    /**
     inspectionShareAtDischargePort should be the name and inspShareAtDP the label
     */

var inspShareAtDischargePort:com.trafigura.edm.shared.types.Percentage = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.trades.PhysicalTradeContractTerms]) {
            val that = r.asInstanceOf[com.trafigura.edm.trades.PhysicalTradeContractTerms]
            that.canEqual(this) && this.contractDate == that.contractDate && this.generalTermsAndConditions == that.generalTermsAndConditions && this.inspShareAtDischargePort == that.inspShareAtDischargePort && this.inspShareAtLoadPort == that.inspShareAtLoadPort && this.notes == that.notes && this.tradeSpec == that.tradeSpec &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.trades.PhysicalTradeContractTerms]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (contractDate == null) 0 else contractDate.hashCode) 
    ) +
         (if (generalTermsAndConditions == null) 0 else generalTermsAndConditions.hashCode) 
    ) +
         (if (inspShareAtDischargePort == null) 0 else inspShareAtDischargePort.hashCode) 
    ) +
         (if (inspShareAtLoadPort == null) 0 else inspShareAtLoadPort.hashCode) 
    ) +
         (if (notes == null) 0 else notes.hashCode) 
    ) +
         (if (tradeSpec == null) 0 else tradeSpec.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.trades.PhysicalTradeContractTerms: " + "" + "contractDate = " + contractDate + ", " + "generalTermsAndConditions = " + generalTermsAndConditions + ", " + "inspShareAtDischargePort = " + inspShareAtDischargePort + ", " + "inspShareAtLoadPort = " + inspShareAtLoadPort + ", " + "notes = " + notes + ", " + "tradeSpec = " + tradeSpec +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.Trades.PhysicalTradeContractTerms@1@1.0")
        
          
          
          
          
            result.putOpt("InspShareAtLoadPort", (this.inspShareAtLoadPort) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("InspShareAtDischargePort", (this.inspShareAtDischargePort) match { case null => null; case o => o.toJson(differentiator) });
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              inspShareAtLoadPort = (jobj.opt("InspShareAtLoadPort")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.shared.types.Percentage.fromJson(o, cache))) }
          
              inspShareAtDischargePort = (jobj.opt("InspShareAtDischargePort")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.shared.types.Percentage.fromJson(o, cache))) }
          
        
    }

  
}

object PhysicalTradeContractTerms {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.Trades.PhysicalTradeContractTerms"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.trades.PhysicalTradeContractTerms = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.trades.PhysicalTradeContractTerms")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.trades.PhysicalTradeContractTerms")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.trades.PhysicalTradeContractTerms()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(inspShareAtLoadPort: com.trafigura.edm.shared.types.Percentage = null, inspShareAtDischargePort: com.trafigura.edm.shared.types.Percentage = null) = {
      val res = new com.trafigura.edm.trades.PhysicalTradeContractTerms
      
        res.inspShareAtLoadPort = inspShareAtLoadPort
      
        res.inspShareAtDischargePort = inspShareAtDischargePort
      
      res
    }
  
}
