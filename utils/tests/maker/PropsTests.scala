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
        assert(MakerProps().ProjectScalaVersion() === "2.9.2")
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

}
