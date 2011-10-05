/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.common.types.datespecifications





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Calendar extends com.trafigura.tradinghub.support.ModelObject  {
  


  
    

var calendarEntries:List[GUID] = null
    
    





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.common.types.datespecifications.Calendar]) {
            val that = r.asInstanceOf[com.trafigura.edm.common.types.datespecifications.Calendar]
            that.canEqual(this) && this.calendarEntries == that.calendarEntries &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.common.types.datespecifications.Calendar]

  override def hashCode = {
    (41 *  1
    ) +
         (if (calendarEntries == null) 0 else calendarEntries.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.common.types.datespecifications.Calendar: " + "" + "calendarEntries = " + calendarEntries +  "}"
  }


  
         def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

     def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = new org.codehaus.jettison.json.JSONObject();

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.Common.Types.DateSpecifications.Calendar@1@1.0")
        
          
          
          
          
            result.putOpt("CalendarEntries", new org.codehaus.jettison.json.JSONArray(if(this.calendarEntries == null) new java.util.ArrayList() else java.util.Arrays.asList(this.calendarEntries.map(t => if (t == null) null else t.toJson).toArray: _*)));
          
        
        return result;
    }

     def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
        
          
              calendarEntries = JSONConversions.optionalList(jobj.opt("CalendarEntries"), x => if (x == null) null else GUID(JSONConversions.optional[String](x, null)))
          
        
    }

  
}

object Calendar {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.Common.Types.DateSpecifications.Calendar"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.common.types.datespecifications.Calendar = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.common.types.datespecifications.Calendar")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.common.types.datespecifications.Calendar")

      
        
            val result = new com.trafigura.edm.common.types.datespecifications.Calendar()
            result.loadJson(jobj, cache)
            result
        
      
    }

  

  

  
    def apply(calendarEntries: List[GUID] = null) = {
      val res = new com.trafigura.edm.common.types.datespecifications.Calendar
      
        res.calendarEntries = calendarEntries
      
      res
    }
  
}
