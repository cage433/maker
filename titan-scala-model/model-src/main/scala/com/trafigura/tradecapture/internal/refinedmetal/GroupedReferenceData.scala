/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.tradecapture.internal.refinedmetal





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class GroupedReferenceData extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var uoms:List[com.trafigura.tradecapture.internal.refinedmetal.UOM] = null
    
    
    

var metals:List[com.trafigura.tradecapture.internal.refinedmetal.Metal] = null
    
    
    

var grades:List[com.trafigura.tradecapture.internal.refinedmetal.Grade] = null
    
    
    

var shapes:List[com.trafigura.tradecapture.internal.refinedmetal.Shape] = null
    
    
    

var currencies:List[com.trafigura.tradecapture.internal.refinedmetal.Currency] = null
    
    
    
    /**
     field 'counterparties',  :list, :element_type => 'Counterparty'
     */

var groupCompanies:List[com.trafigura.tradecapture.internal.refinedmetal.GroupCompany] = null
    
    
    
    /**
     field 'locations',       :list, :element_type => 'Location'
     */

var destionationLocations:List[com.trafigura.tradecapture.internal.refinedmetal.DestinationLocation] = null
    
    
    

var contractualTerms:List[com.trafigura.tradecapture.internal.refinedmetal.ContractualTerms] = null
    
    
    
    /**
     field 'salesTradingRegions',  :list, :element_type => 'SalesTradingRegion'
     */

var securityTerms:List[com.trafigura.tradecapture.internal.refinedmetal.SecurityTerms] = null
    
    
    

var paymentTerms:List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms] = null
    
    
    

var markets:List[com.trafigura.tradecapture.internal.refinedmetal.Market] = null
    
    
    

var pricingOffsets:List[com.trafigura.tradecapture.internal.refinedmetal.PricingOffset] = null
    
    
    

var trafficHubs:List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub] = null
    
    
    

var qptypes:List[com.trafigura.tradecapture.internal.refinedmetal.QPType] = null
    
    
    

var directions:List[com.trafigura.tradecapture.internal.refinedmetal.Direction] = null
    
    
    

var marketLotSizes:List[com.trafigura.tradecapture.internal.refinedmetal.MarketLotSize] = null
    
    
    

var documentTypes:List[com.trafigura.tradecapture.internal.refinedmetal.DocumentType] = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData]) {
            val that = r.asInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData]
            that.canEqual(this) && this.documentTypes == that.documentTypes && this.contractualTerms == that.contractualTerms && this.currencies == that.currencies && this.destionationLocations == that.destionationLocations && this.directions == that.directions && this.grades == that.grades && this.groupCompanies == that.groupCompanies && this.marketLotSizes == that.marketLotSizes && this.markets == that.markets && this.metals == that.metals && this.paymentTerms == that.paymentTerms && this.pricingOffsets == that.pricingOffsets && this.qptypes == that.qptypes && this.securityTerms == that.securityTerms && this.shapes == that.shapes && this.trafficHubs == that.trafficHubs && this.uoms == that.uoms &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData]

  override def hashCode = {
    (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 *  1
    ) +
         (if (documentTypes == null) 0 else documentTypes.hashCode) 
    ) +
         (if (contractualTerms == null) 0 else contractualTerms.hashCode) 
    ) +
         (if (currencies == null) 0 else currencies.hashCode) 
    ) +
         (if (destionationLocations == null) 0 else destionationLocations.hashCode) 
    ) +
         (if (directions == null) 0 else directions.hashCode) 
    ) +
         (if (grades == null) 0 else grades.hashCode) 
    ) +
         (if (groupCompanies == null) 0 else groupCompanies.hashCode) 
    ) +
         (if (marketLotSizes == null) 0 else marketLotSizes.hashCode) 
    ) +
         (if (markets == null) 0 else markets.hashCode) 
    ) +
         (if (metals == null) 0 else metals.hashCode) 
    ) +
         (if (paymentTerms == null) 0 else paymentTerms.hashCode) 
    ) +
         (if (pricingOffsets == null) 0 else pricingOffsets.hashCode) 
    ) +
         (if (qptypes == null) 0 else qptypes.hashCode) 
    ) +
         (if (securityTerms == null) 0 else securityTerms.hashCode) 
    ) +
         (if (shapes == null) 0 else shapes.hashCode) 
    ) +
         (if (trafficHubs == null) 0 else trafficHubs.hashCode) 
    ) +
         (if (uoms == null) 0 else uoms.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData: " + "" + "documentTypes = " + documentTypes + ", " + "contractualTerms = " + contractualTerms + ", " + "currencies = " + currencies + ", " + "destionationLocations = " + destionationLocations + ", " + "directions = " + directions + ", " + "grades = " + grades + ", " + "groupCompanies = " + groupCompanies + ", " + "marketLotSizes = " + marketLotSizes + ", " + "markets = " + markets + ", " + "metals = " + metals + ", " + "paymentTerms = " + paymentTerms + ", " + "pricingOffsets = " + pricingOffsets + ", " + "qptypes = " + qptypes + ", " + "securityTerms = " + securityTerms + ", " + "shapes = " + shapes + ", " + "trafficHubs = " + trafficHubs + ", " + "uoms = " + uoms +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "TradeCapture.Internal.RefinedMetal.GroupedReferenceData@1@1.0")
        
          
          
          
          
            result.putOpt("Uoms", new org.codehaus.jettison.json.JSONArray(if(this.uoms == null) new java.util.ArrayList() else java.util.Arrays.asList(this.uoms.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Metals", new org.codehaus.jettison.json.JSONArray(if(this.metals == null) new java.util.ArrayList() else java.util.Arrays.asList(this.metals.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Grades", new org.codehaus.jettison.json.JSONArray(if(this.grades == null) new java.util.ArrayList() else java.util.Arrays.asList(this.grades.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Shapes", new org.codehaus.jettison.json.JSONArray(if(this.shapes == null) new java.util.ArrayList() else java.util.Arrays.asList(this.shapes.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Currencies", new org.codehaus.jettison.json.JSONArray(if(this.currencies == null) new java.util.ArrayList() else java.util.Arrays.asList(this.currencies.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("GroupCompanies", new org.codehaus.jettison.json.JSONArray(if(this.groupCompanies == null) new java.util.ArrayList() else java.util.Arrays.asList(this.groupCompanies.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("DestionationLocations", new org.codehaus.jettison.json.JSONArray(if(this.destionationLocations == null) new java.util.ArrayList() else java.util.Arrays.asList(this.destionationLocations.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("ContractualTerms", new org.codehaus.jettison.json.JSONArray(if(this.contractualTerms == null) new java.util.ArrayList() else java.util.Arrays.asList(this.contractualTerms.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("SecurityTerms", new org.codehaus.jettison.json.JSONArray(if(this.securityTerms == null) new java.util.ArrayList() else java.util.Arrays.asList(this.securityTerms.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("PaymentTerms", new org.codehaus.jettison.json.JSONArray(if(this.paymentTerms == null) new java.util.ArrayList() else java.util.Arrays.asList(this.paymentTerms.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Markets", new org.codehaus.jettison.json.JSONArray(if(this.markets == null) new java.util.ArrayList() else java.util.Arrays.asList(this.markets.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("PricingOffsets", new org.codehaus.jettison.json.JSONArray(if(this.pricingOffsets == null) new java.util.ArrayList() else java.util.Arrays.asList(this.pricingOffsets.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("TrafficHubs", new org.codehaus.jettison.json.JSONArray(if(this.trafficHubs == null) new java.util.ArrayList() else java.util.Arrays.asList(this.trafficHubs.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Qptypes", new org.codehaus.jettison.json.JSONArray(if(this.qptypes == null) new java.util.ArrayList() else java.util.Arrays.asList(this.qptypes.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("Directions", new org.codehaus.jettison.json.JSONArray(if(this.directions == null) new java.util.ArrayList() else java.util.Arrays.asList(this.directions.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("MarketLotSizes", new org.codehaus.jettison.json.JSONArray(if(this.marketLotSizes == null) new java.util.ArrayList() else java.util.Arrays.asList(this.marketLotSizes.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
          
          
          
          
            result.putOpt("DocumentTypes", new org.codehaus.jettison.json.JSONArray(if(this.documentTypes == null) new java.util.ArrayList() else java.util.Arrays.asList(this.documentTypes.map(t => (t) match { case null => null; case o => o.toJson(differentiator) }).toArray: _*)));
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              uoms = JSONConversions.optionalList(jobj.opt("Uoms"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.UOM.fromJson(o, cache))) })
          
              metals = JSONConversions.optionalList(jobj.opt("Metals"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Metal.fromJson(o, cache))) })
          
              grades = JSONConversions.optionalList(jobj.opt("Grades"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Grade.fromJson(o, cache))) })
          
              shapes = JSONConversions.optionalList(jobj.opt("Shapes"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Shape.fromJson(o, cache))) })
          
              currencies = JSONConversions.optionalList(jobj.opt("Currencies"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Currency.fromJson(o, cache))) })
          
              groupCompanies = JSONConversions.optionalList(jobj.opt("GroupCompanies"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.GroupCompany.fromJson(o, cache))) })
          
              destionationLocations = JSONConversions.optionalList(jobj.opt("DestionationLocations"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.DestinationLocation.fromJson(o, cache))) })
          
              contractualTerms = JSONConversions.optionalList(jobj.opt("ContractualTerms"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.ContractualTerms.fromJson(o, cache))) })
          
              securityTerms = JSONConversions.optionalList(jobj.opt("SecurityTerms"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.SecurityTerms.fromJson(o, cache))) })
          
              paymentTerms = JSONConversions.optionalList(jobj.opt("PaymentTerms"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms.fromJson(o, cache))) })
          
              markets = JSONConversions.optionalList(jobj.opt("Markets"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Market.fromJson(o, cache))) })
          
              pricingOffsets = JSONConversions.optionalList(jobj.opt("PricingOffsets"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.PricingOffset.fromJson(o, cache))) })
          
              trafficHubs = JSONConversions.optionalList(jobj.opt("TrafficHubs"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.TrafficHub.fromJson(o, cache))) })
          
              qptypes = JSONConversions.optionalList(jobj.opt("Qptypes"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.QPType.fromJson(o, cache))) })
          
              directions = JSONConversions.optionalList(jobj.opt("Directions"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.Direction.fromJson(o, cache))) })
          
              marketLotSizes = JSONConversions.optionalList(jobj.opt("MarketLotSizes"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.MarketLotSize.fromJson(o, cache))) })
          
              documentTypes = JSONConversions.optionalList(jobj.opt("DocumentTypes"), x => (x) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.tradecapture.internal.refinedmetal.DocumentType.fromJson(o, cache))) })
          
        
    }

  
}

object GroupedReferenceData {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "TradeCapture.Internal.RefinedMetal.GroupedReferenceData"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData")

      
        
            val result = new com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(uoms: List[com.trafigura.tradecapture.internal.refinedmetal.UOM] = null, metals: List[com.trafigura.tradecapture.internal.refinedmetal.Metal] = null, grades: List[com.trafigura.tradecapture.internal.refinedmetal.Grade] = null, shapes: List[com.trafigura.tradecapture.internal.refinedmetal.Shape] = null, currencies: List[com.trafigura.tradecapture.internal.refinedmetal.Currency] = null, groupCompanies: List[com.trafigura.tradecapture.internal.refinedmetal.GroupCompany] = null, destionationLocations: List[com.trafigura.tradecapture.internal.refinedmetal.DestinationLocation] = null, contractualTerms: List[com.trafigura.tradecapture.internal.refinedmetal.ContractualTerms] = null, securityTerms: List[com.trafigura.tradecapture.internal.refinedmetal.SecurityTerms] = null, paymentTerms: List[com.trafigura.tradecapture.internal.refinedmetal.PaymentTerms] = null, markets: List[com.trafigura.tradecapture.internal.refinedmetal.Market] = null, pricingOffsets: List[com.trafigura.tradecapture.internal.refinedmetal.PricingOffset] = null, trafficHubs: List[com.trafigura.tradecapture.internal.refinedmetal.TrafficHub] = null, qptypes: List[com.trafigura.tradecapture.internal.refinedmetal.QPType] = null, directions: List[com.trafigura.tradecapture.internal.refinedmetal.Direction] = null, marketLotSizes: List[com.trafigura.tradecapture.internal.refinedmetal.MarketLotSize] = null, documentTypes: List[com.trafigura.tradecapture.internal.refinedmetal.DocumentType] = null) = {
      val res = new com.trafigura.tradecapture.internal.refinedmetal.GroupedReferenceData
      
        res.uoms = uoms
      
        res.metals = metals
      
        res.grades = grades
      
        res.shapes = shapes
      
        res.currencies = currencies
      
        res.groupCompanies = groupCompanies
      
        res.destionationLocations = destionationLocations
      
        res.contractualTerms = contractualTerms
      
        res.securityTerms = securityTerms
      
        res.paymentTerms = paymentTerms
      
        res.markets = markets
      
        res.pricingOffsets = pricingOffsets
      
        res.trafficHubs = trafficHubs
      
        res.qptypes = qptypes
      
        res.directions = directions
      
        res.marketLotSizes = marketLotSizes
      
        res.documentTypes = documentTypes
      
      res
    }
  
}
