/**
 * WARNING: This file was automatically generated by the Trading Hub tooling version 2.15.
 * Any manual changes may be overwritten and lost!
 */


// Generated from model_object_scala.erb

package com.trafigura.edm.shared.types





import com.trafigura.tradinghub.support._
import scala.collection.JavaConversions._




 class Currency extends com.trafigura.edm.shared.types.FundamentalUOM  {
  


  





  override def equals(o: Any) = o match {
    case r: AnyRef =>
        if (o.isInstanceOf[com.trafigura.edm.shared.types.Currency]) {
            val that = r.asInstanceOf[com.trafigura.edm.shared.types.Currency]
            that.canEqual(this) && this.name == that.name &&  true
        }
        else false
    case _ => false
  }

  override def canEqual(that : Any) = that.isInstanceOf[com.trafigura.edm.shared.types.Currency]

  override def hashCode = {
    (41 *  1
    ) +
         (if (name == null) 0 else name.hashCode) 
    
  }

  override def toString = {
    "{com.trafigura.edm.shared.types.Currency: " + "" + "name = " + name +  "}"
  }


  
        override def toJson() : org.codehaus.jettison.json.JSONObject = {
      toJson(new SerialisationHelper)
    }

    override def toJson(differentiator: SerialisationHelper) : org.codehaus.jettison.json.JSONObject = {
        var result = super.toJson(differentiator);

        result.put("_node", differentiator.idFor(this))

        result.put("Type", "EDM.shared.types.Currency@1@1.0")
        
        return result;
    }

    override def loadJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper) = {
        
            super.loadJson(jobj, cache);
        
        
          
        
    }

  
}

object Currency {
  def version = ModelVersion("1")
  def wireFormatVersion = ModelVersion("1.0")

  
        val jsonTypeName = "EDM.shared.types.Currency"

    def fromJson(jobj : org.codehaus.jettison.json.JSONObject, cache: DeserialisationHelper = new DeserialisationHelper) : com.trafigura.edm.shared.types.Currency = {
      val v = TypeHolder(jobj.optString("Type"))

      v.assertModelVersion(version, "com.trafigura.edm.shared.types.Currency")
      v.assertWireFormatVersion(wireFormatVersion, "com.trafigura.edm.shared.types.Currency")

      
        
              v.typeName match {
              
                case _ =>
                  
                     val result = new com.trafigura.edm.shared.types.Currency()
                     result.loadJson(jobj, cache)
                     result
                  
            }
        
      
    }

  

  

  
    def apply() = {
      val res = new com.trafigura.edm.shared.types.Currency
      
      res
    }
  
}
