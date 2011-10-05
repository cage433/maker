/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.shared.types





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Date extends com.trafigura.edm.shared.types.DateSpec  {
  


  
    

var value:org.joda.time.LocalDate = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.shared.types.Date]) {
            val that = r.asInstanceOf[com.trafigura.edm.shared.types.Date]
            that.canEqual(this) && this.value == that.value &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.shared.types.Date]

  override def hashCode = {
    (41 *  1
    ) +
         (if (value == null) 0 else value.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.shared.types.Date: " + "" + "value = " + value +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.shared.types.Date@1@1.0")
        
          
          
          
          
            result.putOpt("Value", JSONConversions.optionalDateToJSON(this.value));
          
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
              value = JSONConversions.optionalDate(jobj.opt("Value"))
          
        
    }

  
}

object Date {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.shared.types.Date"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.shared.types.Date = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.shared.types.Date")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.shared.types.Date")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.shared.types.Date()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply(value: org.joda.time.LocalDate = null) = {
      val res = new com.trafigura.edm.shared.types.Date
      
        res.value = value
      
      res
    }
  
}
