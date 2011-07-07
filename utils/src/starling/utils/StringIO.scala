package starling.utils


import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import org.apache.commons.io.FileUtils
import java.net.URL
import io.Source

object StringIO {

  def writeZippedStringToFile(file: File, text: String) {
    try {
      FileUtils.forceMkdir(file.getParentFile)
      val writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(file)))
      writer.write(text)
      writer.close
    } catch {
      case e => throw new RuntimeException(e)
    }
  }
  def writeStringToFile(filename : String, text : String){
    writeZippedStringToFile(new File(filename), text)
  }

  def writePlainStringToFile(file: File, text: String) {
    try {
      FileUtils.forceMkdir(file.getParentFile)
      val writer = new OutputStreamWriter(new FileOutputStream(file))
      writer.write(text)
      writer.close
    } catch {
      case e => throw new RuntimeException(e)
    }
  }

  def readZippedStringFromFile(file : File) : String = {
    val buffer = new StringBuffer
    val iStream = new InputStreamReader(new GZIPInputStream(new FileInputStream(file)))
    val tmp = new Array[Char](4096)
    while(iStream.ready) {
      val read: Int = iStream.read(tmp)
      buffer.append(tmp, 0, read);
    }
    iStream.close
    buffer.toString
  }

  def readStringFromFile(file : File) : String = {
    val buffer = new StringBuffer
    val iStream = new InputStreamReader(new FileInputStream(file))
    val tmp = new Array[Char](4096)
    while(iStream.ready) {
      val read: Int = iStream.read(tmp)
      buffer.append(tmp, 0, read);
    }
    iStream.close
    buffer.toString
  }

  def readStringFromResource(resource : String) = {
    val buffer = new StringBuffer
    val asStream = getClass.getResourceAsStream(resource)
    assert(asStream != null, "Couldn't load resource: " + resource)
    val iStream = new InputStreamReader(asStream)
    val tmp = new Array[Char](4096)
    while(iStream.ready) {
      val read: Int = iStream.read(tmp)
      buffer.append(tmp, 0, read);
    }
    iStream.close
    buffer.toString
  }

  def resource(resource: String) = {
    getClass.getResourceAsStream(resource)
  }

  def url(resource: String) = {
    getClass.getResource(resource)
  }

  def lines(resource: String) = {
    Source.fromURL(getClass.getResource(resource)).getLines
  }
}
