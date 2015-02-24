package maker.utils.os

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import org.scalatest.FunSuite
import maker.utils.FileUtils._

class CommandTests extends FunSuite{

  test("synchronous command runs"){
    withTempDir{
      dir =>
        val f = file(dir, "foo")
        assert(! f.exists)
        val cmd = Command(CommandOutputHandler.NULL, None, true, "touch", f.getAbsolutePath)
        cmd.exec
        assert(f.exists)
    }
  }

  test("asynchronous command runs"){
    withTempDir{
      dir =>
        val f = file(dir, "foo")
        assert(! f.exists)
        val cmd = Command(CommandOutputHandler.NULL, None, true, "touch", f.getAbsolutePath)
        val (_, future) = cmd.execAsync
        val result = Await.result(future, 1 second)
        assert(f.exists)
        assert(result == 0)
    }
  }

  test("Output is written to file"){
    withTempDir{
      dir =>
        val outputFile = file(dir, "output")
        assert(! outputFile.exists)
        val cmd = Command(CommandOutputHandler(outputFile), None, true, "echo", "HELLO")
        cmd.exec
        assert(outputFile.exists)
        val lines = outputFile.readLines.toList
        assert(lines === List("HELLO"))
    }
  }

  test("Output is saved"){
    withTempDir{
      dir =>
        val cmd = Command("echo", "HELLO").withSavedOutput
        cmd.exec
        assert(cmd.savedOutput === "HELLO\n")
    }
  }

  test("Can kill process whose output is being redirected"){
    withTempDir{
      dir => 
        writeToFile(
          file(dir, "main.sh"),
          """
          while true; do
            echo `date`
            sleep 1
          done
          """
        )
        val cmd = Command(Some(dir), "bash", "main.sh").withSavedOutput
        val (proc, future) = cmd.execAsync
        val procID = ProcessID(proc)
        assert(procID.isRunning)
        proc.destroy
        Await.result(future, 10 seconds)
        assert(!procID.isRunning, "Process should have died")
    }
  }


  /**
   * Test is really to ensure my understanding of Java Processes is correct
   */
  test("Can kill process even if its output is not consumed"){
    withTempDir{
      dir => 
        writeToFile(
          file(dir, "main.sh"),
          """
          while true; do
            echo `date`
            sleep 1
          done
          """
        )
        val cmd = new Command(CommandOutputHandler.NO_CONSUME_PROCESS_OUTPUT, Some(dir), true, "bash", "main.sh")
        val (proc, future) = cmd.execAsync
        val procID = ProcessID(proc)
        assert(procID.isRunning)
        proc.destroy
        Await.result(future, 10 seconds)
        assert(!procID.isRunning, "Process should have died")
    }
  }

}
