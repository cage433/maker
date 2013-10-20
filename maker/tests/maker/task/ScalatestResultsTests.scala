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

package maker.task

import org.scalatest.FunSuite
import scala.xml.XML
import scala.xml.MetaData
import scala.xml.Attribute
import java.io.File
import maker.project.{TestModule, Module}
import maker.utils.FileUtils._
import ch.qos.logback.classic.Level._
import maker.Props


class ScalatestResultsTests extends FunSuite{
  def metadataToMap(md : MetaData, acc : Map[String, String] = Map[String, String]()) : Map[String, String] = {
    md match {
      case scala.xml.Null => acc
      case a : Attribute => metadataToMap(md.next, acc ++ Map(a.key -> a.value.toString))
    }
  }

  test("Errors are correctly counted"){
    withTempDir{
      dir =>
        val props = Props.initialiseTestProps(dir)
        val proj = TestModule(dir, "ScalatestResultsTests", props)
        file("maker-resource-config").copyTo(dir)
        proj.writeTest(
          "foo/FooTest.scala",
          """
          package foo
          import org.scalatest.FunSuite
          class FooTest extends FunSuite{
            test("test 1 == 1"){
              assert(1 === 1)
            }
            test("test 5 == 5"){
              assert(1 === 1)
            }
            test("test 1 == 2"){
              assert(1 === 2)
            }
          }
          """
        )

        proj.test
        assert(proj.testResults.numPassedTests() === 2)
        assert(proj.testResults.numFailedTests() === 1)
      }
  }
}
