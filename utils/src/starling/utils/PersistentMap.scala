package starling.utils

import java.io._
import collection.mutable.{HashMap, MapLike}
import scalaz.Scalaz._

trait PersistentCollection{
  def file : File
  protected def collection : Iterable[_]

  protected def readCollection : Option[Object]= {
    if (file.exists) {
      val fis = new FileInputStream(file)
      val ins = new ObjectInputStream(fis)
      val collection = ins.readObject()
      ins.close();
      Some(collection)
    } else {
      None
    }
  }


  protected def persist{
    file.getParentFile().mkdirs()
    val fos = new FileOutputStream(file)
    val out = new ObjectOutputStream(fos);
    out.writeObject(collection)
    out.close()
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

case class PersistentSet[A](file : File) extends scala.collection.mutable.Set[A] with PersistentCollection{

  def this (filename : String) = this(new File(filename))

  private val lock = new Object
  protected var collection = lock.synchronized{
    readCollection.fold(_.asInstanceOf[scala.collection.mutable.Set[A]], scala.collection.mutable.Set[A]())
  }

  def iterator = collection.iterator

  def contains(elem: A) = lock.synchronized{collection.contains(elem)}


  def +=(elem: A) = lock.synchronized{
    collection += elem
    persist
    null
  }

  def -=(elem: A) = lock.synchronized{
    collection -= elem
    persist
    null
  }
}

