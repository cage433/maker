package starling.browser.common

import org.jdesktop.swingx.painter.{AbstractPainter, ImagePainter}
import org.jdesktop.swingx.JXPanel
import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Color, Graphics2D, GradientPaint}

class Painters

class VerticalGradientPaint(topColour: Color, bottomColour: Color) extends AbstractPainter[JXPanel] {
  setAntialiasing(false)
  setCacheable(true)

  def doPaint(g2: Graphics2D, panel: JXPanel, width: Int, height: Int) = {
    val p = new GradientPaint(0, 0, topColour, 0, height, bottomColour)
    g2.setPaint(p)
    g2.fillRect(0, 0, width, height)
  }
}

class ScaledImagePainter(image:BufferedImage) extends ImagePainter(image) {
  setScaleToFit(true)
}

case class StripedCornerPainter(lineColour:Color) extends AbstractPainter[JXPanel] {
  private val r = lineColour.getRed
  private val gr = lineColour.getGreen
  private val b = lineColour.getBlue

  def doPaint(g:Graphics2D, panel:JXPanel, width:Int, height:Int) = {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val spacing = 10
    val num = 20
    for (i <- 0 to num) {
      g.setColor(new Color(r,gr,b, 100 - (100/num) * i))
      g.drawLine(width-i*spacing * 2, height, width,height-i*spacing)
    }
  }
}