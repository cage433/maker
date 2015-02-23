package maker.utils

import maker.utils.RichString._

case class TableBuilder(headings_ : String*){
  private val headings = headings_.toList
  private var rows = List[List[String]]()

  def addRow(items_ : Any*){
    val items = items_.toList.map(_.toString)
    assert(items.size == headings.size, "Wrong number of terms")
    // Don't truncate the last term
    val truncatedItems : List[String] = items.dropRight(1).zip(headings).map{
      case (item, heading) => item.truncate(heading.length - 2).padRight(heading.length)
    } ::: List(items.last)

    rows = rows ::: List(truncatedItems)
  }

  override def toString = {
    val b = new StringBuffer()
    b.addLine(headings.mkString("").inBlue)
    rows.foreach{
      row => 
        b.addLine(row.mkString("").inGreen)
    }
    b.toString
  }
}

