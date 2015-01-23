package maker

import org.scalatest.FunSuite
import scala.collection.mutable.{Map => MMap}
import maker.utils.FileUtils._
import java.io.File

class PropsTests extends FunSuite{


  test("Can't have invalid properties in props file"){
    withTempFile{
      file => 
        writeToFile(file, "InvalidPropKey=jfsfksjfks")
        try{
          MakerProps(file)
          fail("expected exception to be thrown")
        } catch {
          case e : AssertionError => 
        }
    }
  }

  test("Comments are ignored"){
    withTempFile{
      file => 
        writeToFile(file, "#InvalidPropKey=jfsfksjfks")
        assert(MakerProps(file) === MakerProps(MMap[String, String]()))
    }
  }

  test("Properties are overriden"){
    withTempFile{
      file => 
        writeToFile(file, "ProjectScalaVersion=Fred")
        assert(MakerProps(file).ProjectScalaVersion() === "Fred")
        assert(MakerProps().ProjectScalaVersion() === MakerProps.DefaultScalaVersion)
    }
  }

  test("Can override a maker property"){
    val props = MakerProps()
    assert(props.VimErrorFile() != file("fred"))
    props.VimErrorFile := "fred"
    assert(props.VimErrorFile() === file("fred"))
  }

  test("Optional property"){
    class Foo(val overrides : MMap[String, String] = MMap()) extends PropsTrait{
      object Bar extends IsOptionalString
      object Baz extends IsOptionalString
    }
    val foo = new Foo()
    assert(foo.Bar() === None)
    assert(foo.Baz() === None)

    foo.Baz := "fred"
    assert(foo.Baz() === Some("fred"))
  }

  test("reflective toString doesn't throw an exception"){
    object Foo extends PropsTrait{
      val overrides : MMap[String, String] = MMap()
      object Bar extends Default("fred") with IsString
      object Baz extends Default("9") with IsInt
      object Buz extends IsOptionalString
      object SP extends SystemProperty("no-such-property") with IsString
    }
    Foo.toString
    MakerProps().toString
  }

  test("Command lines correct"){
    object Foo extends PropsTrait{
      val overrides : MMap[String, String] = MMap()
      object SP extends SystemPropertyWithDefault("prop", "value") with IsString
    }
    assert(Foo.SP.toCommandLine === "-Dprop=value")
  }

  test("Env property"){

    object Foo extends PropsTrait{
      val overrides : MMap[String, String] = MMap()
      object SP extends EnvProperty("fooble") with IsString
    }
    Foo.SP
  }

}
