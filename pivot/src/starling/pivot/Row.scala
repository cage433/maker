package starling.pivot

import model.{UndefinedValueNew, UndefinedValue}
import starling.quantity._
import starling.utils.sql.PersistAsBlob
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._
import annotation.elidable
import starling.utils.Log


object Row {
  def apply(entries: (Field, Any)*) = new Row(entries.toMap)
  def apply(map: Map[Field, Any], entries: (Field, Any)*): Row = new Row(map) + apply(entries : _*)
  def apply(field: Field, value: Any): Row = apply((field, value))

  def singleRow(rows: List[Row], dataTypeName: String): Row = rows match {
    case row :: Nil => row
    case Nil => throw new Exception("Can't create " + dataTypeName + " from no rows")
    case _ => throw new Exception("Can't create " + dataTypeName + " from more than one row " + rows)
  }

  def create(map: Map[String, Any]): Row = new Row(map.mapKeys(Field(_)))
  def create(rows: Traversable[Map[Field, Any]]): List[Row] = rows.map(Row(_)).toList
}

case class Row(value: Map[Field, Any]) { self =>
  def fields = value.keySet

  lazy val dbValue = new PersistAsBlob(value.mapKeys(_.name).sorted)

  def +(entry: (Field, Any)): Row = copy(value + checkNoDuplicates(entry))
  def +(other: Row) = copy(value ++ checkNoDuplicates(other.value))
  def +?(entry: (Field, Option[Any])): Row = entry._2.fold(any => this + (entry._1 → any), this)
  def +?(entry: Option[(Field, Any)]): Row = entry.fold(this + _, this)
  def ++(other: Map[Field, Any]): Row = copy(value ++ checkNoDuplicates(other))
  def amend(other: Map[Field, Any]): Row = copy(value ++ other)
  def :::?(other: Option[Row]): Row = other.fold(this + _, this)
  def -(key: Field): Row = copy(value - key)
  def updated(key: Field, v: Any): Row = copy(value.updated(key, v))

  def map(f: (Field, Any) => (Field, Any)): Row = copy(value.map(kv => f(kv._1, kv._2)))
  def filterKeys(p: (Field) => Boolean): Row = copy(value.filterKeys(p))

  def matches(field: Field, selection: SomeSelection): Boolean = {
    val v = value.getOrElse(field, UndefinedValue)
    selection.values.contains(v)
  }
  def isEmpty = value.isEmpty

  def isDefined(field: Field) = value.get(field) match {
    case None => false
    case Some(UndefinedValue) => false
    case Some(UndefinedValueNew) => false
    case Some(_) => true
  }

  def apply[T](fieldDetails: FieldDetails): T = apply(fieldDetails.field)
  def apply[T](field: Field): T = value(field).asInstanceOf[T]
  def get[T](fieldDetails: FieldDetails): Option[T] = get(fieldDetails.field)
  def get[T](field: Field): Option[T] = value.get(field).asInstanceOf[Option[T]]

  def double(fieldDetails: FieldDetails): Double = apply[Double](fieldDetails)
  def string(fieldDetails: FieldDetails): String = apply[String](fieldDetails)

  def quantity(fieldDetails: FieldDetails): Quantity = apply[Any](fieldDetails) match {
    case q: Quantity => q
    case pq: PivotQuantity => pq.quantityValue.get
  }

  def pivotQuantity(fieldDetails: FieldDetails): PivotQuantity = apply[Any](fieldDetails) match {
    case q: Quantity => PivotQuantity(q)
    case pq: PivotQuantity => pq
  }

  @inline private def checkNoDuplicates(entry: (Field, Any)): (Field, Any)         = { assertNoDuplicates(entry);   entry   }
  @inline private def checkNoDuplicates(entries: Map[Field, Any]): Map[Field, Any] = { assertNoDuplicates(entries); entries }

  @elidable(elidable.ASSERTION)
  private def assertNoDuplicates(entry: (Field, Any)) = {
    value.get(entry._1).foreach(existingValue => if ((existingValue ?? "") != (entry._2 ?? "")) Log.warn(
      "Cannot add duplicate entry: %s into Row: %s" % (entry, self.toString)))
  }

  @elidable(elidable.ASSERTION)
  private def assertNoDuplicates(entries: Map[Field, Any]): Unit = entries.foreach(assertNoDuplicates)
}