package maker.task.compile

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.os.Command
import org.apache.commons.exec.DefaultExecuteResultHandler
import java.io.{InputStream, BufferedInputStream, ByteArrayInputStream}
import scala.language.reflectiveCalls

class ReplTests extends FreeSpec with Matchers {

  "Can talk to repl" in {
    /**
      * Hacky and completely non-thread safe repl simulator
      */
    val inputStream = new InputStream(){

      private var pointer: Int = 0
      var text: String = ""

      override def available() = text.size - pointer

      override def read() = {

        if (available() == 0) {
          -1
        } else {

          val c = text(pointer.toInt)
          pointer += 1
          c
        }
      }

      def inputLine(line: String) {
        text = text + s"$line\n"
      }

    }

    val bis = new BufferedInputStream(inputStream) {
      def inputLine(line: String) {
        inputStream.inputLine(line)
      }
    }
    val streamHandler = new ReplTestPumpStreamHandler(System.out, System.err, inputStream)
    val repl = Command(
      overrideOutput = None,
      timeout = None, 
      overrideStreamHandler = Some(streamHandler),
      args = Seq("scala")
    )
    val resultHandler = new DefaultExecuteResultHandler()
    repl.runAsync(resultHandler)
    Thread.sleep(1000)
    inputStream.inputLine(s"""println("Hi")""");
    Thread.sleep(1000)
    inputStream.inputLine(s"""println("Hi again")""");
    Thread.sleep(3000)
    inputStream.inputLine(s"""println("Hi once more")""");
    Thread.sleep(1000)
    inputStream.inputLine(s""":quit""");
    resultHandler.waitFor
  }
}
