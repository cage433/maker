package starling.utils

import java.io.{FileInputStream, File}
import java.security.MessageDigest

object IOUtils {

  def md5(file:File) = {
    val bytes = createChecksum(file);
    val result = new StringBuilder()
    for (b <- bytes) {
      result.append(Integer.toString( ( b & 0xff ) + 0x100, 16).substring( 1 ))
    }
    result.toString;
  }

  private def createChecksum(file:File) = {
    val fis =  new FileInputStream(file)

    val buffer = new Array[Byte](1024);
    val complete = MessageDigest.getInstance("MD5");
    var numRead:Int = 0
    do {
     numRead = fis.read(buffer)
     if (numRead > 0) {
       complete.update(buffer, 0, numRead)
     }
    } while (numRead != -1)
    fis.close()
    complete.digest();
  }
}
