package maker

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import java.io.File
import org.omg.CORBA.DynAnyPackage.Invalid
import scala.collection.immutable.Nil

class Props2(overrides : Map[String, String]){

  object Compiler extends RegularProperty with IsString with HasDefaultValue {
    def default = "zinc"
  }

  object JavaHome extends EnvironmentProperty with IsFile with IsRequired{
    def variableNames = "JAVA_HOME" :: "JDK_HOME" :: Nil
  }

  object ExecMode extends SystemProperty with IsBoolean with HasDefaultValue{
    def propertyKey = "maker.execmode"
    def default = false
  }

  trait MakerProperty{
    type T
    type P // Either `T` or `Option[T]` - depending on whether the property is optional
    
    protected def fromString(s : String) : T
    protected def optionalValue : Option[T]
    def apply() : P
    def overrideValue : Option[T] = overrides.get(name).map(fromString(_))
    def name = {
      val objectName = getClass.getName
      objectName.split('$').last
    }
  }

  // 'Regular' <==> 'Not Environment or system property'
  trait RegularProperty extends MakerProperty{
    def optionalValue = overrideValue
  }
  trait EnvironmentProperty extends MakerProperty{
    def variableNames : Seq[String]
    protected def optionalValue = variableNames.foldLeft(overrideValue){
      case (Some(value), _) => Some(value)
      case (None, variableName) => getenv(variableName).map(fromString(_))
    }
  }
  // method to enable testing of EnvironmentProperty
  protected def getenv(variableName : String) : Option[String] = Option(System.getenv(variableName))

  trait SystemProperty extends MakerProperty{
    def propertyKey : String
    protected def optionalValue = overrideValue orElse Option(System.getProperty(propertyKey)).map(fromString(_))
  }

  trait IsRequired{
    self : MakerProperty => 
      type P = self.T
      def apply() = optionalValue match {
        case Some(v) => v
        case None => 
          throw new IllegalStateException(s"Property ${self.name} is not defined")
      }
  }

  trait HasDefaultValue{
    self : MakerProperty => 
      type P = self.T
      def default : self.P
      def apply() = optionalValue.getOrElse(default) 
  }

  trait IsString{
    self : MakerProperty => 
      type T = String
      protected def fromString(s : String) = s
  }

  trait IsFile{
    self : MakerProperty => 
      type T = File
      protected def fromString(s : String) = file(s).asAbsoluteFile
  }

  trait IsBoolean{
    self : MakerProperty => 
      type T = Boolean
      protected def fromString(s : String) = s.toBoolean
  }
  val propertyMethods = this.getClass.getMethods.filter{
    m =>
      classOf[MakerProperty].isAssignableFrom(m.getReturnType) && m.getParameterTypes.isEmpty
  }
  private def checkForInvalidProperties(){
    overrides.foreach{
      case (o, _) => 
      require(propertyMethods.map(_.getName).toSet.contains(o), "Overiding non existant property " + o)
    }
  }
  checkForInvalidProperties()
}

object Props2{
  def apply() = new Props2(Map.empty)
  def apply(key1 : String, value1 : String, moreKeysAndValues : String*) : Props2 = {
    new Props2(Map(key1 -> value1) ++ moreKeysAndValues.toList.grouped(2).map{
      case List(k, v) => k -> v
    })
  }
}
class Props2Tests extends FreeSpec with Matchers{
  "Invalid properties" in {
    intercept[Exception]{
      Props2("fooble", "blah2")
    }
    
  }

  "Regular property" in {
    Props2().Compiler() should be (Props2().Compiler.default)
    Props2("Compiler", "foo").Compiler() should be ("foo")
  }

  "Envionment based property in " in {
    val map = scala.collection.mutable.Map[String, String]()
    val props = new Props2(Map.empty){
      protected override def getenv(variableName : String) = map.get(variableName)
    }

    intercept[IllegalStateException]{
      props.JavaHome()
    }
    map("JAVA_HOME") = "foo"
    props.JavaHome() should be (file("foo").asAbsoluteFile)
    map("JAVA_HOME") = "/foo/bar/java"
    props.JavaHome() should be (file("/foo/bar/java"))

    new Props2(Map("JavaHome" -> "fooble")).JavaHome() should be (file("fooble").asAbsoluteFile)
  }

  "System based property" in {
    val propertyKey = Props2().ExecMode.propertyKey
    System.clearProperty(propertyKey)

    info("default")
    Props2().ExecMode() should be (Props2().ExecMode.default)

    info("override")
    Props2("ExecMode", "true").ExecMode() should be (true)
    Props2("ExecMode", "false").ExecMode() should be (false)

    info("using system property")
    System.setProperty(propertyKey, "true")
    Props2().ExecMode() should be (true)
    System.setProperty(propertyKey, "false")
    Props2().ExecMode() should be (false)

    info("override takes precedence over system")
    Props2("ExecMode", "true").ExecMode() should be (true)
    Props2("ExecMode", "false").ExecMode() should be (false)
  }
}
