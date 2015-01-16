package maker.utils

import java.io.{OutputStream, File}
import java.io.FileOutputStream
import java.io.PrintStream
import org.apache.commons.io.output.TeeOutputStream

case class TeeToFileOutputStream(file : File, os : OutputStream = Console.out) extends OutputStream {
  protected def makeTeeStream = {
    new PrintStream(
      new TeeOutputStream(
        os,
        new PrintStream(new FileOutputStream(file))
      ),
      true
    )
  }

  var tee = makeTeeStream
  def write(b : Int){
    tee.write(b)
  }
  override def flush(){
    tee.flush
    os.flush
  }
}
