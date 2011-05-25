package starling.pivot.view.swing

import org.jdesktop.swingx.JXTable
import javax.swing.table.TableModel
import javax.swing.{UIManager, JTable}

class AxisJTable(tableModel:TableModel) extends JXTable(tableModel) {
  setBackground(UIManager.getColor("Panel.background"))
  setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
  getTableHeader.setReorderingAllowed(false)
  setCellSelectionEnabled(false)
  setFocusable(false)
  setTableHeader(null)

  override def doFind = {}
}