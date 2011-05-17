package starling.gui.pages

import java.awt.Dimension
import starling.pivot.view.swing.MigPanel
import starling.gui._
import starling.utils.{SColumn, STable}

class FullScreenReportPage(title:String, tableAsString:String) extends Page {
  def text = title
  val icon = StarlingIcons.im("/icons/stock_chart-reorganize.png")
  def build(reader:PageBuildingContext) = {
    val ls = System.getProperty("line.separator")    
    val data = tableAsString.split(ls).toList.map(_.split("\t").toList)
    val colCount = {
      if (data.length > 0) {
        data(0).length
      } else {
        0
      }
    }
    val columns = List.fill(colCount)(TableTable.EmptyColumnHeader).map(SColumn(_))
    FullScreenReportPageData(new STable(title, columns, data))
  }
  def createComponent(context:PageContext, data:PageData, browserSize:Dimension) = new FullScreenReportComponent(data)
}

case class FullScreenReportPageData(table:STable) extends PageData

class FullScreenReportComponent(pageData:PageData) extends MigPanel("insets 0") with PageComponent {
  private val data = pageData match {case d:FullScreenReportPageData => d}
  val table = new TableTable(data.table)
  add(table, "push, grow")

  override def getBorder = None
}