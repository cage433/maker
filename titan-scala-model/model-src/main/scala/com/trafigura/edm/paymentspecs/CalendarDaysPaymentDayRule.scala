/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.paymentspecs





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class CalendarDaysPaymentDayRule extends com.trafigura.edm.paymentspecs.PaymentDayRule  {
  


  
    

var ruleIfSaturday:com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null
    
    
    

var ruleIfSunday:com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null
    
    
    

var ruleIfHolidayMonday:com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null
    
    
    

var ruleIfOtherMonday:com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule]) {
            val that = r.asInstanceOf[com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule]
            that.canEqual(this) && this.ruleIfHolidayMonday == that.ruleIfHolidayMonday && this.ruleIfOtherMonday == that.ruleIfOtherMonday && this.ruleIfSaturday == that.ruleIfSaturday && this.ruleIfSunday == that.ruleIfSunday &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule]

  override def hashCode = {
    (41 * (41 * (41 * (41 *  1
    ) +
         (if (ruleIfHolidayMonday == null) 0 else ruleIfHolidayMonday.hashCode) 
    ) +
         (if (ruleIfOtherMonday == null) 0 else ruleIfOtherMonday.hashCode) 
    ) +
         (if (ruleIfSaturday == null) 0 else ruleIfSaturday.hashCode) 
    ) +
         (if (ruleIfSunday == null) 0 else ruleIfSunday.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule: " + "" + "ruleIfHolidayMonday = " + ruleIfHolidayMonday + ", " + "ruleIfOtherMonday = " + ruleIfOtherMonday + ", " + "ruleIfSaturday = " + ruleIfSaturday + ", " + "ruleIfSunday = " + ruleIfSunday +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.PaymentSpecs.CalendarDaysPaymentDayRule@1@1.0")
        
          
          
          
          
            result.putOpt("RuleIfSaturday", (this.ruleIfSaturday) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("RuleIfSunday", (this.ruleIfSunday) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("RuleIfHolidayMonday", (this.ruleIfHolidayMonday) match { case null => null; case o => o.toJson(differentiator) });
          
        
          
          
          
          
            result.putOpt("RuleIfOtherMonday", (this.ruleIfOtherMonday) match { case null => null; case o => o.toJson(differentiator) });
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              ruleIfSaturday = (jobj.opt("RuleIfSaturday")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.paymentspecs.PaymentDayHolidayRule.fromJson(o, cache))) }
          
              ruleIfSunday = (jobj.opt("RuleIfSunday")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.paymentspecs.PaymentDayHolidayRule.fromJson(o, cache))) }
          
              ruleIfHolidayMonday = (jobj.opt("RuleIfHolidayMonday")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.paymentspecs.PaymentDayHolidayRule.fromJson(o, cache))) }
          
              ruleIfOtherMonday = (jobj.opt("RuleIfOtherMonday")) match { case null => null; case o: org.codehaus.jettison.json.JSONObject => cache.objectFor(o).getOrElse(cache.seen(o, com.trafigura.edm.paymentspecs.PaymentDayHolidayRule.fromJson(o, cache))) }
          
        
    }

  
}

object CalendarDaysPaymentDayRule {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.PaymentSpecs.CalendarDaysPaymentDayRule"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(ruleIfSaturday: com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null, ruleIfSunday: com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null, ruleIfHolidayMonday: com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null, ruleIfOtherMonday: com.trafigura.edm.paymentspecs.PaymentDayHolidayRule = null) = {
      val res = new com.trafigura.edm.paymentspecs.CalendarDaysPaymentDayRule
      
        res.ruleIfSaturday = ruleIfSaturday
      
        res.ruleIfSunday = ruleIfSunday
      
        res.ruleIfHolidayMonday = ruleIfHolidayMonday
      
        res.ruleIfOtherMonday = ruleIfOtherMonday
      
      res
    }
  
}
