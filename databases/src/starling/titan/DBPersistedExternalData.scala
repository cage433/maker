package starling.titan

import starling.instrument.utils.StarlingXStream
import starling.utils.ImplicitConversions._
import starling.db.{DBTrait, DBWriter, DB}
import starling.richdb.RichResultSetRow


trait ExternalTitanData[A] {

  def clear

  def ++=(x: Iterable[(String, A)])

  def retain(pred: (String, A) => Boolean)

  def values: Seq[A]

  def ids: Set[String]

  def get(id: String): Option[A]

  def +=(pair: (String, A))

  def -=(id: String)

  def getOrElse(id: String, default: A) = get(id) match {
    case Some(x) => x
    case None => default
  }

  def getOrDie(id: String) = get(id) match {
    case Some(x) => x
    case None => throw new Exception("No value found for id " + id)
  }

}

import starling.dbx.QueryBuilder._

case class DBPersistedExternalTitanData[A](db: DBTrait[RichResultSetRow], table: String, keyCol: String, valueCol: String) extends ExternalTitanData[A] {
  private val lock = new Object
  private var map = Map[String, A]()
  private val MAX_IN_CLAUSE_SIZE = 1000 // SQL Server doesn't like big 'in' clauses

  db.query("""select * from %s""" % table, Map[String, A]()) {
    rs =>
      val key = rs.getString(keyCol)
      val value = StarlingXStream.read(rs.getString(valueCol)).asInstanceOf[A]
      map += (key -> value)
  }

  def clear = lock.synchronized {
    map = Map[String, A]()
    db.inTransaction {
      writer =>
        writer.update("truncate table %s" % table)
    }
  }

  def ++=(xs: Iterable[(String, A)]) = lock.synchronized {
    db.inTransaction {
      writer =>
        delete(xs.map(_._1))
        xs.foreach {
          case (key, value) =>
            println("Inserting " + key + ", " + value)
            writer.insert(table, Map(keyCol -> key, valueCol -> StarlingXStream.write(value.asInstanceOf[AnyRef])))
        }
    }
    xs.foreach {
      case (key, value) =>
        map += key -> value
    }
  }

  private def delete(keys: Iterable[String]) {
    db.inTransaction {
      writer =>
        keys.grouped(MAX_IN_CLAUSE_SIZE).foreach {
          group =>
            writer.delete(table, keyCol in group)
        }
    }
  }

  def retain(p: (String, A) => Boolean) = lock.synchronized {
    delete(map.filter(p).keySet)
  }

  def values = lock.synchronized{
    map.values.toList
  }

  def ids = lock.synchronized{
    map.keySet
  }

  def get(id: String) = lock.synchronized{
    map.get(id)
  }

  def +=(pair: (String, A)) = ++= (List(pair))

  def -=(id: String) = retain{case (id_, _ ) => id != id_}
}
