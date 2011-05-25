package starling.gui

import org.jdesktop.jxlayer.plaf.ext.LockableUI
import org.jdesktop.jxlayer.JXLayer
import java.awt.geom.{AffineTransform, Point2D}
import java.awt.event.ActionListener
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.painter.RectanglePainter
import javax.swing.{JButton, JProgressBar, JLabel, JComponent}
import java.awt._
import org.jdesktop.swingx.{JXBusyLabel, JXPanel}

class LockedProgressUI(text: String, actionListener: ActionListener) extends LockableUI {
  private val progressPanel = new ProgressPanel(text, actionListener)
  progressPanel.setVisible(false)

  def updateText(text: String) {
    progressPanel.updateText(text)
    setDirty(true)
  }

  /*override def installUI(c: JComponent) = {
    super.installUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.setLayout(new MigLayout)
    glassPane.add(progressPanel, "center, push")
    progressPanel.setCursor(Cursor.getDefaultCursor)
  }

  override def uninstallUI(c: JComponent) = {
    super.uninstallUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.setLayout(new FlowLayout)
    glassPane.remove(progressPanel)
  }

  override def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = {
    super.paintLayer(g2, l)
    if (isLocked) {
      val width = l.getWidth
      val height = l.getHeight
      val origPaint = g2.getPaint
      g2.setPaint(new RadialGradientPaint(new Point2D.Double(width / 2, width / 2), width * 0.75f,
        new Point2D.Double(width / 2, width / 2), Array(0.0f, 0.7f, 1.0f),
        Array(new Color(0, 0, 0, 128), new Color(0, 0, 0, 64), new Color(0, 0, 0, 0)),
        MultipleGradientPaint.CycleMethod.NO_CYCLE, MultipleGradientPaint.ColorSpaceType.SRGB,
        AffineTransform.getScaleInstance(1.0, height.asInstanceOf[Double] / width.asInstanceOf[Double])))
      g2.fillRect(0, 0, width, height)
      g2.setPaint(origPaint)
    }
  }

  override def setLocked(locked: Boolean) = {
    super.setLocked(locked)
    progressPanel.busyLabel.setBusy(locked)
    progressPanel.setVisible(locked)
  }*/


  private class ProgressPanel(text: String, actionListener: ActionListener) extends JXPanel(new MigLayout) {
    setOpaque(false)
    val busyLabel = new JXBusyLabel
    updateText(text)
    
    add(busyLabel, "growx")
    if (actionListener != null) {
      val cancelButton = new JButton("Cancel")
      cancelButton.addActionListener(actionListener)
      add(cancelButton)
    }
    private val rectanglePainter: RectanglePainter = new RectanglePainter(0, 0, getHeight, getWidth, 10, 10, true, getBackground, 1, Color.BLACK)
    rectanglePainter.setAntialiasing(true)
    setBackgroundPainter(rectanglePainter)

    def updateText(text: String): Unit = {
      busyLabel.setText("  " + text + "    ")
    }
  }
}