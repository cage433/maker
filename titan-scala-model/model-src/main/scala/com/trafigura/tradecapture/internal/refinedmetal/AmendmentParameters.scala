/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class AmendmentParameters extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var currentVersion:com.trafigura.tradecapture.internal.refinedmetal.Version = null
    
    
    

var amendmentVersion:com.trafigura.tradecapture.internal.refinedmetal.Version = null
    
    
    

var contractVersion:com.trafigura.tradecapture.internal.refinedmetal.Version = null
    
    
    

var currentParameters:com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null
    
    
    

var amendmendParameters:com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null
    
    
    

var contractParameters:com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null
    
    
    

var cdocResult:Boolean = false
    
    
    

var docAmendmentsDisabled:Boolean = false
    
    
    

var reqTranslations:List[com.trafigura.tradecapture.internal.refinedmetal.RichTranslation] = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters]
            that.canEqual(this) && this.amendmendParameters == that.amendmendParameters && this.amendmentVersion == that.amendmentVersion && this.cdocResult == that.cdocResult && this.contractParameters == that.contractParameters && this.contractVersion == that.contractVersion && this.currentParameters == that.currentParameters && this.currentVersion == that.currentVersion && this.docAmendmentsDisabled == that.docAmendmentsDisabled && this.reqTranslations == that.reqTranslations &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (amendmendParameters == null) 0 else amendmendParameters.hashCode) 
    ) +
         (if (amendmentVersion == null) 0 else amendmentVersion.hashCode) 
    ) +
         cdocResult.hashCode
        
    ) +
         (if (contractParameters == null) 0 else contractParameters.hashCode) 
    ) +
         (if (contractVersion == null) 0 else contractVersion.hashCode) 
    ) +
         (if (currentParameters == null) 0 else currentParameters.hashCode) 
    ) +
         (if (currentVersion == null) 0 else currentVersion.hashCode) 
    ) +
         docAmendmentsDisabled.hashCode
        
    ) +
         (if (reqTranslations == null) 0 else reqTranslations.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters: " + "" + "amendmendParameters = " + amendmendParameters + ", " + "amendmentVersion = " + amendmentVersion + ", " + "cdocResult = " + cdocResult + ", " + "contractParameters = " + contractParameters + ", " + "contractVersion = " + contractVersion + ", " + "currentParameters = " + currentParameters + ", " + "currentVersion = " + currentVersion + ", " + "docAmendmentsDisabled = " + docAmendmentsDisabled + ", " + "reqTranslations = " + reqTranslations +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.AmendmentParameters@1@1.0")
        
          
          
          
          
            result.putOpt("CurrentVersion", (this.currentVersion) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("AmendmentVersion", (this.amendmentVersion) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("ContractVersion", (this.contractVersion) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("CurrentParameters", (this.currentParameters) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("AmendmendParameters", (this.amendmendParameters) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("ContractParameters", (this.contractParameters) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("CdocResult", this.cdocResult);
          
        
          
          
          
          
            result.putOpt("DocAmendmentsDisabled", this.docAmendmentsDisabled);
          
        
          
          
          
          
            result.putOpt("ReqTranslations", new org.codehaus.jettison.json.JSONArray(if(this.reqTranslations == null) new java.util.ArrayList() else java.util.Arrays.asList(this.reqTranslations.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              currentVersion = (jobj.opt("CurrentVersion")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Version.fromJson(o, cache))) }
          
              amendmentVersion = (jobj.opt("AmendmentVersion")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Version.fromJson(o, cache))) }
          
              contractVersion = (jobj.opt("ContractVersion")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Version.fromJson(o, cache))) }
          
              currentParameters = (jobj.opt("CurrentParameters")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters.fromJson(o, cache))) }
          
              amendmendParameters = (jobj.opt("AmendmendParameters")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters.fromJson(o, cache))) }
          
              contractParameters = (jobj.opt("ContractParameters")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters.fromJson(o, cache))) }
          
              cdocResult = JSONConversions.optional[Boolean](jobj.opt("CdocResult"), false)
          
              docAmendmentsDisabled = JSONConversions.optional[Boolean](jobj.opt("DocAmendmentsDisabled"), false)
          
              reqTranslations = JSONConversions.optionalList(jobj.opt("ReqTranslations"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.RichTranslation.fromJson(o, cache))) })
          
        
    }

  
}

object AmendmentParameters {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.AmendmentParameters"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(currentVersion: com.trafigura.tradecapture.internal.refinedmetal.Version = null, amendmentVersion: com.trafigura.tradecapture.internal.refinedmetal.Version = null, contractVersion: com.trafigura.tradecapture.internal.refinedmetal.Version = null, currentParameters: com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null, amendmendParameters: com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null, contractParameters: com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = null, cdocResult: Boolean = false, docAmendmentsDisabled: Boolean = false, reqTranslations: List[com.trafigura.tradecapture.internal.refinedmetal.RichTranslation] = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.AmendmentParameters
      
        res.currentVersion = currentVersion
      
        res.amendmentVersion = amendmentVersion
      
        res.contractVersion = contractVersion
      
        res.currentParameters = currentParameters
      
        res.amendmendParameters = amendmendParameters
      
        res.contractParameters = contractParameters
      
        res.cdocResult = cdocResult
      
        res.docAmendmentsDisabled = docAmendmentsDisabled
      
        res.reqTranslations = reqTranslations
      
      res
    }
  
}
