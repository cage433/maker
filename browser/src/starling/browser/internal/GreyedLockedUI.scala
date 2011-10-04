package starling.browser.internal

import org.jdesktop.jxlayer.plaf.ext.LockableUI
import javax.swing.JComponent
import org.jdesktop.jxlayer.JXLayer
import java.awt.geom.{Point2D, AffineTransform}
import java.awt.{RadialGradientPaint, Color, MultipleGradientPaint, Graphics2D}
import starling.browser.common.GuiUtils

class GreyedLockedUI extends LockableUI {
  override def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = {
    super.paintLayer(g2, l)
    if (isLocked) {
      val width = l.getWidth
      val height = l.getHeight
      val origPaint = g2.getPaint
      g2.setPaint(new RadialGradientPaint(new Point2D.Double(width / 2, width / 2), width * 0.75f,
        new Point2D.Double(width / 2, width / 2), Array(0.0f, 0.7f, 1.0f),
        Array(new Color(0, 0, 0, 128), new Color(0, 0, 0, 64), GuiUtils.ClearColour),
        MultipleGradientPaint.CycleMethod.NO_CYCLE, MultipleGradientPaint.ColorSpaceType.SRGB,
        AffineTransform.getScaleInstance(1.0, height.asInstanceOf[Double] / width.asInstanceOf[Double])))
      g2.fillRect(0, 0, width, height)
      g2.setPaint(origPaint)
    }
  }

  override def setLocked(locked: Boolean) = {
    super.setLocked(locked)
    setDirty(true)
  }
}