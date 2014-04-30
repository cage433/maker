/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker

import maker.utils.FileUtils._
import maker.utils.RichString._
import java.io.File
import scala.collection.mutable.{Map ⇒ MMap}
import ch.qos.logback.classic.Level
import xml.{XML, NodeSeq}
import scala.util.Properties

trait PropsTrait extends DelayedInit{
  protected def overrides : MMap[String, String]

  protected def checkForInvalidProperties{
    overrides.foreach{
      case (o, _) => 
      assert(propertyMethods.map(_.getName).toSet.contains(o), "Overiding non existant property " + o)
    }
  }
  /**
   DelayedInit ensures overrides are constructed before 
   their values are checked
  */
  def delayedInit(x : ⇒ Unit){
    x
    checkForInvalidProperties
  }
  val propertyMethods = this.getClass.getMethods.filter{
    m =>
      classOf[Property].isAssignableFrom(m.getReturnType) && m.getParameterTypes.isEmpty
  }

  override def toString = {
    val buffer = new StringBuffer
    propertyMethods.foreach(m =>
        try {
          buffer.append(m.invoke(this) + "\n")
        } catch {
          case e ⇒ 
            buffer.append(m.getName + " threw " + e + "\n")
        }
      )
    buffer.toString
  }

  class PropertyNotSetException(key : String) extends Throwable("Property " + key +  " not set")


  trait Property{
    type T
    def stringValue : String = overrides.getOrElse(name, throw new PropertyNotSetException(name))
    def apply() : T 
    def name = {
      val objectName = getClass.getName
      objectName.split('$').last
    }
    override def toString = {
      val valueAsString = try {
        apply().toString
      } catch {
        case _ : PropertyNotSetException ⇒ "Property no set"
      }
      name + "=" + valueAsString
    }
    def := (newValue : String){
      overrides += (name -> newValue)
    }
    def := (newValue : T){
      overrides += (name -> newValue.toString)
    }
  }

  abstract class Default(default : ⇒ Any) extends Property{
    override def stringValue = overrides.getOrElse(name, default.toString)
  }

  abstract class SystemProperty(key : String) extends Property{
    protected def systemValue = Properties.propOrNone(key)
    override def stringValue = overrides.getOrElse(name, systemValue.getOrElse{throw new Exception("Required System property " + name + " not set")})
    def toCommandLine(value : String) = "-D%s=%s" % (key, value)
    def toCommandLine = "-D%s=%s" % (key, apply())
    def toCommandLine(appender : T ⇒ String) = "-D%s=%s" % (key, appender(apply()))
  }

  abstract class SystemPropertyWithDefault(key : String, default : Any) extends SystemProperty(key){
    override def stringValue = {
      overrides.getOrElse(name, systemValue.getOrElse(default.toString))
    }
  }

  trait IsString{
    self: Property ⇒ 
    type T = String
    def apply() = self.stringValue
  }

  trait IsFile{
    self: Property ⇒ 
    type T = File
    def apply() = file(self.stringValue)
  }


  trait IsBoolean{
    self: Property ⇒ 
    type T = Boolean
    def apply() = java.lang.Boolean.parseBoolean(stringValue)
  }

  trait IsXml{
    self: Property ⇒
    type T = NodeSeq
    def apply() = XML.loadString(self.stringValue)
  }

  trait IsOptionalString extends Property{
    type T = Option[String]
    override def stringValue = throw new UnsupportedOperationException()
    def apply() = overrides.get(name)
  }
  trait IsOptionalFile extends Property{
    type T = Option[File]
    override def stringValue = throw new UnsupportedOperationException()
    def apply() = overrides.get(name).map(file)
  }
  trait IsInt{
    self: Property ⇒ 
    type T = Int
    def apply() = self.stringValue.toInt
  }
  trait IsLogLevel{
    self: Property ⇒ 
    type T = Level
    def apply() = Level.toLevel(stringValue)
  }

  class EmptyString extends Default("") with IsString
  
  abstract class EnvProperty(vars : String*) extends Property{
    override def stringValue = vars.toList.flatMap{
      v ⇒ Option(System.getenv(v))
    }.headOption.getOrElse{throw new PropertyNotSetException(vars.toList.mkString(","))}
  }
}
