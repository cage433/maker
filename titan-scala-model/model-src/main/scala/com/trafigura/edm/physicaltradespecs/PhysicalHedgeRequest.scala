/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.physicaltradespecs





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._



/**
 Represents a physical hedge request (in lots)
 */


 class PhysicalHedgeRequest extends com.trafigura.edm.physicaltradespecs.EDMHedgeRequest  {
  


  
    

var lots:scala.Option[Int] = None
    
    
    

var neptuneId:scala.Option[Int] = None
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest]) {
            val that = r.asInstanceOf[com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest]
            that.canEqual(this) && this.contractMonthName == that.contractMonthName && this.direction == that.direction && this.hedgePrice == that.hedgePrice && this.lots == that.lots && this.market == that.market && this.neptuneId == that.neptuneId && this.ordinal == that.ordinal &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (contractMonthName == null) 0 else contractMonthName.hashCode) 
    ) +
         (if (direction == null) 0 else direction.hashCode) 
    ) +
         (if (hedgePrice == null) 0 else hedgePrice.hashCode) 
    ) +
         (if (lots == null) 0 else lots.hashCode) 
    ) +
         (if (market == null) 0 else market.hashCode) 
    ) +
         (if (neptuneId == null) 0 else neptuneId.hashCode) 
    ) +
         ordinal.hashCode
        
    
  }

  override def toString = {
    "{com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest: " + "" + "contractMonthName = " + contractMonthName + ", " + "direction = " + direction + ", " + "hedgePrice = " + hedgePrice + ", " + "lots = " + lots + ", " + "market = " + market + ", " + "neptuneId = " + neptuneId + ", " + "ordinal = " + ordinal +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.PhysicalTradeSpecs.PhysicalHedgeRequest@1@1.0")
        
          
          
          
          
            result.putOpt("Lots", this.lots.getOrElse(null));
          
        
          
          
          
          
            result.putOpt("NeptuneId", this.neptuneId.getOrElse(null));
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              lots = (jobj.opt("Lots")) match {case null => None; case n:Number => Some(n.intValue()); case _ => None}
          
              neptuneId = (jobj.opt("NeptuneId")) match {case null => None; case n:Number => Some(n.intValue()); case _ => None}
          
        
    }

  
}

object PhysicalHedgeRequest {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.PhysicalTradeSpecs.PhysicalHedgeRequest"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(lots: scala.Option[Int] = None, neptuneId: scala.Option[Int] = None) = {
      val res = new com.trafigura.edm.physicaltradespecs.PhysicalHedgeRequest
      
        res.lots = lots
      
        res.neptuneId = neptuneId
      
      res
    }
  
}
