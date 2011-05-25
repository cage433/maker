package starling.gui.custom

import swing.TextArea
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{JComponent, JScrollPane, JViewport, JTextArea}

class AutoResizingTextArea(text0:String="", rows0:Int=0, columns0:Int=0) extends TextArea {
  override lazy val peer = new JTextArea(text0, rows0, columns0) with SuperMixin {
    var _maxRows = 1
    var _minRows = 1

    def minRows = _minRows
    def minRows_=(mr:Int) = {
      _minRows = mr
      setRows(clipRowCount(getRows))
    }

    def maxRows = _maxRows
    def maxRows_=(mr:Int) = {
      _maxRows = mr
      setRows(clipRowCount(getRows))
    }



    getDocument.addDocumentListener(new DocumentListener {
      def changedUpdate(e:DocumentEvent) = {updateSize(e)}
      def removeUpdate(e:DocumentEvent) = {updateSize(e)}
      def insertUpdate(e:DocumentEvent) = {updateSize(e)}
    })

    override def setRows(rows:Int) = {
      val oldRow = super.getRows
      val newRow = clipRowCount(rows)
      super.setRows(newRow)

      numberOrRowsUpdated(oldRow, newRow)
    }

    def updateSize(e:DocumentEvent) {
      val roots = e.getDocument.getRootElements
      val root = roots(0)
      val rowCount = root.getElementCount
      setRows(clipRowCount(rowCount))
    }

    def clipRowCount(rows:Int) = {
      val r = math.min(_maxRows, rows)
      math.max(_minRows, r)
    }

    def numberOrRowsUpdated(oldRow:Int, newRow:Int) {
      val scroll = getParentScrollPane
      if (scroll != null) {
        val parent = scroll.getParent
        if (parent != null && parent.isInstanceOf[JComponent]) {
          parent.asInstanceOf[JComponent].revalidate
        }
      }
    }

    def getParentScrollPane:JScrollPane = {
      val parent = getParent
      if (parent != null && parent.isInstanceOf[JViewport]) {
        parent.getParent.asInstanceOf[JScrollPane]
      } else {
        null
      }
    }
  }
}