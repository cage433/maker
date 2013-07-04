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

package maker.utils.os

import org.scalatest.FunSuite
import maker.utils.FileUtils._
import scala.actors.Futures
import maker.MakerProps

class CommandTests extends FunSuite{

  val props = MakerProps()
  test("synchronous command runs"){
    withTempDir{
      dir ⇒
        val f = file(dir, "foo")
        assert(! f.exists)
        val cmd = Command(props, CommandOutputHandler.NULL, None, "touch", f.getAbsolutePath)
        cmd.exec
        assert(f.exists)
    }
  }

  test("asynchronous command runs"){
    withTempDir{
      dir ⇒
        val f = file(dir, "foo")
        assert(! f.exists)
        val cmd = Command(props, CommandOutputHandler.NULL, None, "touch", f.getAbsolutePath)
        val (_, future) = cmd.execAsync
        val result = Futures.awaitAll(1000, future).head
        assert(f.exists)
        assert(result == Some(0))
    }
  }

  test("Output is written to file"){
    withTempDir{
      dir ⇒
        val outputFile = file(dir, "output")
        assert(! outputFile.exists)
        val cmd = Command(props, CommandOutputHandler(outputFile), None, "echo", "HELLO")
        cmd.exec
        assert(outputFile.exists)
        val lines = outputFile.readLines.toList
        assert(lines === List("HELLO"))
    }
  }

  test("Output is saved"){
    withTempDir{
      dir ⇒
        val cmd = Command(props, CommandOutputHandler.NULL.withSavedOutput, None, "echo", "HELLO")
        cmd.exec
        assert(cmd.savedOutput === "HELLO\n")
    }
  }

  test("Can kill process whose output is being redirected"){
    withTempDir{
      dir ⇒ 
        writeToFile(
          file(dir, "main.sh"),
          """
          while true; do
            echo `date`
            sleep 1
          done
          """
        )
        val cmd = new Command(props, new CommandOutputHandler().withSavedOutput, Some(dir), "bash", "main.sh")
        val (proc, future) = cmd.execAsync
        val procID = ProcessID(proc)
        assert(procID.isRunning(props))
        assert(! future.isSet)
        proc.destroy
        Futures.awaitAll(10000, future)
        assert(!procID.isRunning(props), "Process should have died")
        assert(future.isSet)
    }
  }


  /**
   * Test is really to ensure my understanding of Java Processes is correct
   */
  test("Can kill process even if its output is not consumed"){
    withTempDir{
      dir ⇒ 
        writeToFile(
          file(dir, "main.sh"),
          """
          while true; do
            echo `date`
            sleep 1
          done
          """
        )
        val cmd = new Command(props, CommandOutputHandler.NO_CONSUME_PROCESS_OUTPUT, Some(dir), "bash", "main.sh")
        val (proc, future) = cmd.execAsync
        val procID = ProcessID(proc)
        assert(procID.isRunning(props))
        assert(! future.isSet)
        proc.destroy
        Futures.awaitAll(10000, future)
        assert(!procID.isRunning(props), "Process should have died")
        assert(future.isSet)
    }
  }

}
