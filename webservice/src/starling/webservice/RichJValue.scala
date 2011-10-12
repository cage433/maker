package starling.webservice

import net.liftweb.json._
import starling.utils.ImplicitConversions._


object RichJValue {
  implicit def enrichJValue(jvalue: JValue) = new RichJValue(jvalue)

  class RichJValue(jvalue: JValue) {
    def capitalize   = jvalue.transform { case jfield: JField => jfield.copy(jfield.name.capitalize)   }
    def uncapitalize = jvalue.transform { case jfield: JField => jfield.copy(jfield.name.uncapitalize) }
  }
}