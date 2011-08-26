package starling.gui.pages

import starling.utils.STable
import javax.swing.{JTable, JLabel, JScrollPane}
import org.jdesktop.swingx.JXTable
import javax.swing.table.AbstractTableModel
import starling.pivot.TableCell

object TableTable {
  val EmptyColumnHeader = "sdkjhsdfjkhsjkh"
}
class TableTable(table:STable) extends JScrollPane {
  val model = new AbstractTableModel {
    override def getRowCount = table.data.size
    override def getColumnCount = table.columns.size
    override def getColumnName(column: Int) = {
      if (table.columns(column).name == TableTable.EmptyColumnHeader) {
        " "
      } else {
        table.columns(column).name.trim
      }
    }
    override def getValueAt(row: Int, column: Int) = table.data(row)(column).asInstanceOf[AnyRef]
    // note: this may not work - but probably does for all the classes that we're putting into STable at the moment
    override def getColumnClass(p1: Int) = classOf[Comparable[_]]
  }
  val jTable = new JXTable(model) {
    setFillsViewportHeight(true)
    setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
    val tmpLabel = new JLabel("") {
      setFont(getFont.deriveFont(java.awt.Font.BOLD))
    }

    getTableHeader.setReorderingAllowed(false)
    setSortable(false)

    val maxWidth = 300
    for (col <- 0 until getColumnCount) {
      var width = 80
      for (value <- table.columns(col).name :: table.data.map{_(col)}) {
        val textToUse = value match {
          case tc:TableCell => tc.text
          case other => other.toString
        }
        tmpLabel.setText(textToUse)
        width = math.max(width, tmpLabel.getPreferredSize.width)
      }
      width = math.min(width + 5, maxWidth)
      columnModel.getColumn(col).setPreferredWidth(width)
      columnModel.getColumn(col).setMinWidth(width)
    }

    override def doFind = {}
  }
  setViewportView(jTable)
//  jTable.setPreferredScrollableViewportSize(new Dimension(jTable.getPreferredSize.width, jTable.getPreferredSize.height))
}