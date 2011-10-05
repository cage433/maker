/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class ContractDocumentParameters extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var version:org.joda.time.DateTime = null
    
    
    

var draft:Boolean = false
    
    
    

var neptuneId:String = null
    
    
    

var completeNeptuneId:String = null
    
    
    

var counterpartyName:String = null
    
    
    

var groupCompanyCode:String = null
    
    
    

var nonKYCCounterparty:Boolean = false
    
    
    

var counterpartyAddress:String = null
    
    
    

var counterpartyPostcode:String = null
    
    
    

var counterpartyPhoneNumber:String = null
    
    
    

var counterpartyFaxNumber:String = null
    
    
    

var counterpartyContactName:String = null
    
    
    

var counterpartyAccountNumber:String = null
    
    
    

var traderName:String = null
    
    
    

var purchase:Boolean = false
    
    
    

var contractDate:org.joda.time.LocalDate = null
    
    
    

var groupCompany:String = null
    
    
    

var hub:String = null
    
    
    

var totalQty:Double = 0.0
    
    
    

var uom:String = null
    
    
    

var tolerance:Double = 0.0
    
    
    

var comments:String = null
    
    
    

var metal:String = null
    
    
    

var grade:String = null
    
    
    

var shape:String = null
    
    
    

var gradeComments:String = null
    
    
    

var contractualLocation:String = null
    
    
    

var contractualTerms:String = null
    
    
    

var paymentTerms:String = null
    
    
    

var securityTerms:String = null
    
    
    

var market:String = null
    
    
    

var currency:String = null
    
    
    

var premium:Double = 0.0
    
    
    

var premiumCcy:String = null
    
    
    

var premiumUom:String = null
    
    
    

var qpType:String = null
    
    
    

var pricingOffset:String = null
    
    
    

var quotas:List[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentQuotaParameters] = null
    
    
    

var metalContractSuffix:String = null
    
    
    

var hubCode:String = null
    
    
    

var sourceSystem:String = null
    
    
    

var auditModifiedByName:String = null
    
    
    

var titanId:String = null
    
    
    

var creditDays:Int = 0
    
    
    

var tradeId:Int = 0
    
    
    

var contractGenerationDate:org.joda.time.DateTime = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters]
            that.canEqual(this) && this.auditModifiedByName == that.auditModifiedByName && this.comments == that.comments && this.completeNeptuneId == that.completeNeptuneId && this.contractDate == that.contractDate && this.contractGenerationDate == that.contractGenerationDate && this.contractualLocation == that.contractualLocation && this.contractualTerms == that.contractualTerms && this.counterpartyAccountNumber == that.counterpartyAccountNumber && this.counterpartyAddress == that.counterpartyAddress && this.counterpartyContactName == that.counterpartyContactName && this.counterpartyFaxNumber == that.counterpartyFaxNumber && this.counterpartyName == that.counterpartyName && this.counterpartyPhoneNumber == that.counterpartyPhoneNumber && this.counterpartyPostcode == that.counterpartyPostcode && this.creditDays == that.creditDays && this.currency == that.currency && this.draft == that.draft && this.grade == that.grade && this.gradeComments == that.gradeComments && this.groupCompany == that.groupCompany && this.groupCompanyCode == that.groupCompanyCode && this.hub == that.hub && this.hubCode == that.hubCode && this.market == that.market && this.metal == that.metal && this.metalContractSuffix == that.metalContractSuffix && this.neptuneId == that.neptuneId && this.nonKYCCounterparty == that.nonKYCCounterparty && this.paymentTerms == that.paymentTerms && this.premium == that.premium && this.premiumCcy == that.premiumCcy && this.premiumUom == that.premiumUom && this.pricingOffset == that.pricingOffset && this.purchase == that.purchase && this.qpType == that.qpType && this.quotas == that.quotas && this.securityTerms == that.securityTerms && this.shape == that.shape && this.sourceSystem == that.sourceSystem && this.titanId == that.titanId && this.tolerance == that.tolerance && this.totalQty == that.totalQty && this.tradeId == that.tradeId && this.traderName == that.traderName && this.uom == that.uom && this.version == that.version &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (auditModifiedByName == null) 0 else auditModifiedByName.hashCode) 
    ) +
         (if (comments == null) 0 else comments.hashCode) 
    ) +
         (if (completeNeptuneId == null) 0 else completeNeptuneId.hashCode) 
    ) +
         (if (contractDate == null) 0 else contractDate.hashCode) 
    ) +
         (if (contractGenerationDate == null) 0 else contractGenerationDate.hashCode) 
    ) +
         (if (contractualLocation == null) 0 else contractualLocation.hashCode) 
    ) +
         (if (contractualTerms == null) 0 else contractualTerms.hashCode) 
    ) +
         (if (counterpartyAccountNumber == null) 0 else counterpartyAccountNumber.hashCode) 
    ) +
         (if (counterpartyAddress == null) 0 else counterpartyAddress.hashCode) 
    ) +
         (if (counterpartyContactName == null) 0 else counterpartyContactName.hashCode) 
    ) +
         (if (counterpartyFaxNumber == null) 0 else counterpartyFaxNumber.hashCode) 
    ) +
         (if (counterpartyName == null) 0 else counterpartyName.hashCode) 
    ) +
         (if (counterpartyPhoneNumber == null) 0 else counterpartyPhoneNumber.hashCode) 
    ) +
         (if (counterpartyPostcode == null) 0 else counterpartyPostcode.hashCode) 
    ) +
         creditDays.hashCode
        
    ) +
         (if (currency == null) 0 else currency.hashCode) 
    ) +
         draft.hashCode
        
    ) +
         (if (grade == null) 0 else grade.hashCode) 
    ) +
         (if (gradeComments == null) 0 else gradeComments.hashCode) 
    ) +
         (if (groupCompany == null) 0 else groupCompany.hashCode) 
    ) +
         (if (groupCompanyCode == null) 0 else groupCompanyCode.hashCode) 
    ) +
         (if (hub == null) 0 else hub.hashCode) 
    ) +
         (if (hubCode == null) 0 else hubCode.hashCode) 
    ) +
         (if (market == null) 0 else market.hashCode) 
    ) +
         (if (metal == null) 0 else metal.hashCode) 
    ) +
         (if (metalContractSuffix == null) 0 else metalContractSuffix.hashCode) 
    ) +
         (if (neptuneId == null) 0 else neptuneId.hashCode) 
    ) +
         nonKYCCounterparty.hashCode
        
    ) +
         (if (paymentTerms == null) 0 else paymentTerms.hashCode) 
    ) +
         premium.hashCode
        
    ) +
         (if (premiumCcy == null) 0 else premiumCcy.hashCode) 
    ) +
         (if (premiumUom == null) 0 else premiumUom.hashCode) 
    ) +
         (if (pricingOffset == null) 0 else pricingOffset.hashCode) 
    ) +
         purchase.hashCode
        
    ) +
         (if (qpType == null) 0 else qpType.hashCode) 
    ) +
         (if (quotas == null) 0 else quotas.hashCode) 
    ) +
         (if (securityTerms == null) 0 else securityTerms.hashCode) 
    ) +
         (if (shape == null) 0 else shape.hashCode) 
    ) +
         (if (sourceSystem == null) 0 else sourceSystem.hashCode) 
    ) +
         (if (titanId == null) 0 else titanId.hashCode) 
    ) +
         tolerance.hashCode
        
    ) +
         totalQty.hashCode
        
    ) +
         tradeId.hashCode
        
    ) +
         (if (traderName == null) 0 else traderName.hashCode) 
    ) +
         (if (uom == null) 0 else uom.hashCode) 
    ) +
         (if (version == null) 0 else version.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters: " + "" + "auditModifiedByName = " + auditModifiedByName + ", " + "comments = " + comments + ", " + "completeNeptuneId = " + completeNeptuneId + ", " + "contractDate = " + contractDate + ", " + "contractGenerationDate = " + contractGenerationDate + ", " + "contractualLocation = " + contractualLocation + ", " + "contractualTerms = " + contractualTerms + ", " + "counterpartyAccountNumber = " + counterpartyAccountNumber + ", " + "counterpartyAddress = " + counterpartyAddress + ", " + "counterpartyContactName = " + counterpartyContactName + ", " + "counterpartyFaxNumber = " + counterpartyFaxNumber + ", " + "counterpartyName = " + counterpartyName + ", " + "counterpartyPhoneNumber = " + counterpartyPhoneNumber + ", " + "counterpartyPostcode = " + counterpartyPostcode + ", " + "creditDays = " + creditDays + ", " + "currency = " + currency + ", " + "draft = " + draft + ", " + "grade = " + grade + ", " + "gradeComments = " + gradeComments + ", " + "groupCompany = " + groupCompany + ", " + "groupCompanyCode = " + groupCompanyCode + ", " + "hub = " + hub + ", " + "hubCode = " + hubCode + ", " + "market = " + market + ", " + "metal = " + metal + ", " + "metalContractSuffix = " + metalContractSuffix + ", " + "neptuneId = " + neptuneId + ", " + "nonKYCCounterparty = " + nonKYCCounterparty + ", " + "paymentTerms = " + paymentTerms + ", " + "premium = " + premium + ", " + "premiumCcy = " + premiumCcy + ", " + "premiumUom = " + premiumUom + ", " + "pricingOffset = " + pricingOffset + ", " + "purchase = " + purchase + ", " + "qpType = " + qpType + ", " + "quotas = " + quotas + ", " + "securityTerms = " + securityTerms + ", " + "shape = " + shape + ", " + "sourceSystem = " + sourceSystem + ", " + "titanId = " + titanId + ", " + "tolerance = " + tolerance + ", " + "totalQty = " + totalQty + ", " + "tradeId = " + tradeId + ", " + "traderName = " + traderName + ", " + "uom = " + uom + ", " + "version = " + version +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.ContractDocumentParameters@1@1.0")
        
          
          
          
          
            result.putOpt("Version", JSONConversions.optionalDatetimeToJSON(this.version));
          
        
          
          
          
          
            result.putOpt("Draft", this.draft);
          
        
          
          
          
          
            result.putOpt("NeptuneId", this.neptuneId);
          
        
          
          
          
          
            result.putOpt("CompleteNeptuneId", this.completeNeptuneId);
          
        
          
          
          
          
            result.putOpt("CounterpartyName", this.counterpartyName);
          
        
          
          
          
          
            result.putOpt("GroupCompanyCode", this.groupCompanyCode);
          
        
          
          
          
          
            result.putOpt("NonKYCCounterparty", this.nonKYCCounterparty);
          
        
          
          
          
          
            result.putOpt("CounterpartyAddress", this.counterpartyAddress);
          
        
          
          
          
          
            result.putOpt("CounterpartyPostcode", this.counterpartyPostcode);
          
        
          
          
          
          
            result.putOpt("CounterpartyPhoneNumber", this.counterpartyPhoneNumber);
          
        
          
          
          
          
            result.putOpt("CounterpartyFaxNumber", this.counterpartyFaxNumber);
          
        
          
          
          
          
            result.putOpt("CounterpartyContactName", this.counterpartyContactName);
          
        
          
          
          
          
            result.putOpt("CounterpartyAccountNumber", this.counterpartyAccountNumber);
          
        
          
          
          
          
            result.putOpt("TraderName", this.traderName);
          
        
          
          
          
          
            result.putOpt("Purchase", this.purchase);
          
        
          
          
          
          
            result.putOpt("ContractDate", JSONConversions.optionalDateToJSON(this.contractDate));
          
        
          
          
          
          
            result.putOpt("GroupCompany", this.groupCompany);
          
        
          
          
          
          
            result.putOpt("Hub", this.hub);
          
        
          
          
          
          
            result.putOpt("TotalQty", this.totalQty);
          
        
          
          
          
          
            result.putOpt("Uom", this.uom);
          
        
          
          
          
          
            result.putOpt("Tolerance", this.tolerance);
          
        
          
          
          
          
            result.putOpt("Comments", this.comments);
          
        
          
          
          
          
            result.putOpt("Metal", this.metal);
          
        
          
          
          
          
            result.putOpt("Grade", this.grade);
          
        
          
          
          
          
            result.putOpt("Shape", this.shape);
          
        
          
          
          
          
            result.putOpt("GradeComments", this.gradeComments);
          
        
          
          
          
          
            result.putOpt("ContractualLocation", this.contractualLocation);
          
        
          
          
          
          
            result.putOpt("ContractualTerms", this.contractualTerms);
          
        
          
          
          
          
            result.putOpt("PaymentTerms", this.paymentTerms);
          
        
          
          
          
          
            result.putOpt("SecurityTerms", this.securityTerms);
          
        
          
          
          
          
            result.putOpt("Market", this.market);
          
        
          
          
          
          
            result.putOpt("Currency", this.currency);
          
        
          
          
          
          
            result.putOpt("Premium", this.premium);
          
        
          
          
          
          
            result.putOpt("PremiumCcy", this.premiumCcy);
          
        
          
          
          
          
            result.putOpt("PremiumUom", this.premiumUom);
          
        
          
          
          
          
            result.putOpt("QpType", this.qpType);
          
        
          
          
          
          
            result.putOpt("PricingOffset", this.pricingOffset);
          
        
          
          
          
          
            result.putOpt("Quotas", new org.codehaus.jettison.json.JSONArray(if(this.quotas == null) new java.util.ArrayList() else java.util.Arrays.asList(this.quotas.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("MetalContractSuffix", this.metalContractSuffix);
          
        
          
          
          
          
            result.putOpt("HubCode", this.hubCode);
          
        
          
          
          
          
            result.putOpt("SourceSystem", this.sourceSystem);
          
        
          
          
          
          
            result.putOpt("AuditModifiedByName", this.auditModifiedByName);
          
        
          
          
          
          
            result.putOpt("TitanId", this.titanId);
          
        
          
          
          
          
            result.putOpt("CreditDays", this.creditDays);
          
        
          
          
          
          
            result.putOpt("TradeId", this.tradeId);
          
        
          
          
          
          
            result.putOpt("ContractGenerationDate", JSONConversions.optionalDatetimeToJSON(this.contractGenerationDate));
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              version = JSONConversions.optionalDatetime(jobj.opt("Version"))
          
              draft = JSONConversions.optional[Boolean](jobj.opt("Draft"), false)
          
              neptuneId = JSONConversions.optional[String](jobj.opt("NeptuneId"), null)
          
              completeNeptuneId = JSONConversions.optional[String](jobj.opt("CompleteNeptuneId"), null)
          
              counterpartyName = JSONConversions.optional[String](jobj.opt("CounterpartyName"), null)
          
              groupCompanyCode = JSONConversions.optional[String](jobj.opt("GroupCompanyCode"), null)
          
              nonKYCCounterparty = JSONConversions.optional[Boolean](jobj.opt("NonKYCCounterparty"), false)
          
              counterpartyAddress = JSONConversions.optional[String](jobj.opt("CounterpartyAddress"), null)
          
              counterpartyPostcode = JSONConversions.optional[String](jobj.opt("CounterpartyPostcode"), null)
          
              counterpartyPhoneNumber = JSONConversions.optional[String](jobj.opt("CounterpartyPhoneNumber"), null)
          
              counterpartyFaxNumber = JSONConversions.optional[String](jobj.opt("CounterpartyFaxNumber"), null)
          
              counterpartyContactName = JSONConversions.optional[String](jobj.opt("CounterpartyContactName"), null)
          
              counterpartyAccountNumber = JSONConversions.optional[String](jobj.opt("CounterpartyAccountNumber"), null)
          
              traderName = JSONConversions.optional[String](jobj.opt("TraderName"), null)
          
              purchase = JSONConversions.optional[Boolean](jobj.opt("Purchase"), false)
          
              contractDate = JSONConversions.optionalDate(jobj.opt("ContractDate"))
          
              groupCompany = JSONConversions.optional[String](jobj.opt("GroupCompany"), null)
          
              hub = JSONConversions.optional[String](jobj.opt("Hub"), null)
          
              totalQty = JSONConversions.optionalReal(jobj.opt("TotalQty"))
          
              uom = JSONConversions.optional[String](jobj.opt("Uom"), null)
          
              tolerance = JSONConversions.optionalReal(jobj.opt("Tolerance"))
          
              comments = JSONConversions.optional[String](jobj.opt("Comments"), null)
          
              metal = JSONConversions.optional[String](jobj.opt("Metal"), null)
          
              grade = JSONConversions.optional[String](jobj.opt("Grade"), null)
          
              shape = JSONConversions.optional[String](jobj.opt("Shape"), null)
          
              gradeComments = JSONConversions.optional[String](jobj.opt("GradeComments"), null)
          
              contractualLocation = JSONConversions.optional[String](jobj.opt("ContractualLocation"), null)
          
              contractualTerms = JSONConversions.optional[String](jobj.opt("ContractualTerms"), null)
          
              paymentTerms = JSONConversions.optional[String](jobj.opt("PaymentTerms"), null)
          
              securityTerms = JSONConversions.optional[String](jobj.opt("SecurityTerms"), null)
          
              market = JSONConversions.optional[String](jobj.opt("Market"), null)
          
              currency = JSONConversions.optional[String](jobj.opt("Currency"), null)
          
              premium = JSONConversions.optionalReal(jobj.opt("Premium"))
          
              premiumCcy = JSONConversions.optional[String](jobj.opt("PremiumCcy"), null)
          
              premiumUom = JSONConversions.optional[String](jobj.opt("PremiumUom"), null)
          
              qpType = JSONConversions.optional[String](jobj.opt("QpType"), null)
          
              pricingOffset = JSONConversions.optional[String](jobj.opt("PricingOffset"), null)
          
              quotas = JSONConversions.optionalList(jobj.opt("Quotas"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentQuotaParameters.fromJson(o, cache))) })
          
              metalContractSuffix = JSONConversions.optional[String](jobj.opt("MetalContractSuffix"), null)
          
              hubCode = JSONConversions.optional[String](jobj.opt("HubCode"), null)
          
              sourceSystem = JSONConversions.optional[String](jobj.opt("SourceSystem"), null)
          
              auditModifiedByName = JSONConversions.optional[String](jobj.opt("AuditModifiedByName"), null)
          
              titanId = JSONConversions.optional[String](jobj.opt("TitanId"), null)
          
              creditDays = JSONConversions.optional[Int](jobj.opt("CreditDays"), 0)
          
              tradeId = JSONConversions.optional[Int](jobj.opt("TradeId"), 0)
          
              contractGenerationDate = JSONConversions.optionalDatetime(jobj.opt("ContractGenerationDate"))
          
        
    }

  
}

object ContractDocumentParameters {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.ContractDocumentParameters"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(version: org.joda.time.DateTime = null, draft: Boolean = false, neptuneId: String = null, completeNeptuneId: String = null, counterpartyName: String = null, groupCompanyCode: String = null, nonKYCCounterparty: Boolean = false, counterpartyAddress: String = null, counterpartyPostcode: String = null, counterpartyPhoneNumber: String = null, counterpartyFaxNumber: String = null, counterpartyContactName: String = null, counterpartyAccountNumber: String = null, traderName: String = null, purchase: Boolean = false, contractDate: org.joda.time.LocalDate = null, groupCompany: String = null, hub: String = null, totalQty: Double = 0.0, uom: String = null, tolerance: Double = 0.0, comments: String = null, metal: String = null, grade: String = null, shape: String = null, gradeComments: String = null, contractualLocation: String = null, contractualTerms: String = null, paymentTerms: String = null, securityTerms: String = null, market: String = null, currency: String = null, premium: Double = 0.0, premiumCcy: String = null, premiumUom: String = null, qpType: String = null, pricingOffset: String = null, quotas: List[com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentQuotaParameters] = null, metalContractSuffix: String = null, hubCode: String = null, sourceSystem: String = null, auditModifiedByName: String = null, titanId: String = null, creditDays: Int = 0, tradeId: Int = 0, contractGenerationDate: org.joda.time.DateTime = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.ContractDocumentParameters
      
        res.version = version
      
        res.draft = draft
      
        res.neptuneId = neptuneId
      
        res.completeNeptuneId = completeNeptuneId
      
        res.counterpartyName = counterpartyName
      
        res.groupCompanyCode = groupCompanyCode
      
        res.nonKYCCounterparty = nonKYCCounterparty
      
        res.counterpartyAddress = counterpartyAddress
      
        res.counterpartyPostcode = counterpartyPostcode
      
        res.counterpartyPhoneNumber = counterpartyPhoneNumber
      
        res.counterpartyFaxNumber = counterpartyFaxNumber
      
        res.counterpartyContactName = counterpartyContactName
      
        res.counterpartyAccountNumber = counterpartyAccountNumber
      
        res.traderName = traderName
      
        res.purchase = purchase
      
        res.contractDate = contractDate
      
        res.groupCompany = groupCompany
      
        res.hub = hub
      
        res.totalQty = totalQty
      
        res.uom = uom
      
        res.tolerance = tolerance
      
        res.comments = comments
      
        res.metal = metal
      
        res.grade = grade
      
        res.shape = shape
      
        res.gradeComments = gradeComments
      
        res.contractualLocation = contractualLocation
      
        res.contractualTerms = contractualTerms
      
        res.paymentTerms = paymentTerms
      
        res.securityTerms = securityTerms
      
        res.market = market
      
        res.currency = currency
      
        res.premium = premium
      
        res.premiumCcy = premiumCcy
      
        res.premiumUom = premiumUom
      
        res.qpType = qpType
      
        res.pricingOffset = pricingOffset
      
        res.quotas = quotas
      
        res.metalContractSuffix = metalContractSuffix
      
        res.hubCode = hubCode
      
        res.sourceSystem = sourceSystem
      
        res.auditModifiedByName = auditModifiedByName
      
        res.titanId = titanId
      
        res.creditDays = creditDays
      
        res.tradeId = tradeId
      
        res.contractGenerationDate = contractGenerationDate
      
      res
    }
  
}
