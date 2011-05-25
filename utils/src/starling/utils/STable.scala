package starling.utils

/**
 * A standard table with typed columns and a fixed number of rows.
 *
 * This gets used in JTables and PivotTables
 */

case class STable(name:String, columns:List[SColumn], data:List[List[Any]]) {
  def apply(row:Int, column:String) = {
    val columnIndex = columns.map(_.name).indexOf(column)
    data(row)(columnIndex)
  }
  override def toString = {
    val buffer = new StringBuilder
    buffer.append(name + ": " + columns.size + " columns " + data.size + " rows\n")
    buffer.append("  | " + columns.map(_.name).mkString(", ") + "\n")
    for (row <- data) {
      buffer.append(row.size + " | " + row.map(_.toString).mkString(", ") + "\n")
    }
    buffer.toString
  }
}

case class SColumn(name:String, columnType:SColumnType) {
  def sqlName = name.replaceAll(" ", "_")
}
object SColumn {
  def apply(name:String) = new SColumn(name, StringColumnType)
}

class SColumnType
case object StringColumnType extends SColumnType
case object LongStringColumnType extends SColumnType
case object DoubleColumnType extends SColumnType
case object QuantityColumnType extends SColumnType
case object LongColumnType extends SColumnType


