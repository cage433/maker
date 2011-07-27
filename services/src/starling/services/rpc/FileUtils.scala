package starling.services.rpc

import java.net.URL
import io.Source._
import com.trafigura.tradinghub.support.ModelObject
import org.codehaus.jettison.json.JSONObject
import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}


/**
 * Some simple file utils for handling json mock data etc,
 *   used for edm service mocks etc
 */
object FileUtils {
  def getFileUrl(file : String) = getClass.getResource(file)

  def loadJsonValuesFromFileUrl(fileUrl : URL, compress : Boolean = false) : List[String] = {
    //println("load json from file url " + fileUrl)
    val bufferSize = 512 * 1024
    if (!compress) {
      fromURL(fileUrl).getLines.toList
    }
    else {
      try {
        val fStream = new FileInputStream(new File(fileUrl.toURI))
        var inStream = if (compress) {
          new BufferedInputStream(new GZIPInputStream(fStream), bufferSize)
        }
        else {
          new BufferedInputStream(fStream)
        }

        var data = ""
        var bytesRead = 0
        val dataBuffer = Array.ofDim[Byte](bufferSize)
        while ({ bytesRead = inStream.read(dataBuffer); bytesRead != -1 }) {
          //println("read %d bytes".format(bytesRead))
          if (bytesRead > 0) {
            val dataChunk = new String(dataBuffer, 0, bytesRead)
            data += dataChunk
          }
        }
        val lines : List[String] = data.split("\n").toList
        lines
      }
      catch {
        case ex : Exception => println("Error: " + ex.getMessage()); throw ex
      }
    }
  }

  def loadJsonValuesFromFile(filePath : String, compress : Boolean = false) : List[String] =
    loadJsonValuesFromFileUrl(new File(filePath).toURI.toURL, compress)

  def writeJson[T <: ModelObject with Object { def toJson() : JSONObject }](fileName : String, objects : List[T], compress : Boolean = false) {

    try {
      val fStream = new FileOutputStream(fileName)
      var outStream = if (compress) {
        new BufferedOutputStream(new GZIPOutputStream(fStream))
      }
      else {
        new BufferedOutputStream(fStream)
      }

      objects.foreach(obj => outStream.write((obj.toJson() + "\n").getBytes()))
      outStream.flush()
      outStream.close()
    }
    catch {
      case ex : Exception => println("Error: " + ex.getMessage())
    }
  }
}
