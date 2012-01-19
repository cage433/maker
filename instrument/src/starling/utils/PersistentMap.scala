package starling.utils

//import java.io.
import collection.mutable.{HashMap, MapLike}
import scalaz.Scalaz._
import starling.instrument.utils.StarlingXStream
import java.io.{ObjectOutputStream, FileOutputStream, FileInputStream, File}

trait PersistentCollection{
  def file : File
  protected def collection : Iterable[_]

  protected def readCollection : Option[Object]= {
    if (file.exists) {
      val fis: FileInputStream = new FileInputStream(file)
      val text = org.apache.commons.io.IOUtils.toString(fis)
      fis.close
      Some(StarlingXStream.read(text).asInstanceOf[AnyRef])
    } else {
      None
    }
  }


  protected def persist{
    file.getParentFile().mkdirs()
    val fos = new FileOutputStream(file)
    org.apache.commons.io.IOUtils.write(StarlingXStream.write(collection), fos)
    fos.close()
  }
}

case class PersistentMap[A, B](file : File) extends scala.collection.mutable.Map[A, B] with PersistentCollection{

  def this (filename : String) = this(new File(filename))

  private val lock = new Object
  protected var collection = lock.synchronized{
    readCollection.fold(_.asInstanceOf[scala.collection.mutable.Map[A, B]], scala.collection.mutable.Map[A, B]())
  }

  def iterator = collection.iterator

  def get(key: A) = lock.synchronized{
    collection.get(key)
  }

  def +=(kv: (A, B)) = lock.synchronized{
    collection += (kv)
    persist
    null    // Get type error if I try to return this
  }

  def -=(key: A) = lock.synchronized{
    collection -= (key)
    persist
    null
  }
}




