package starling.browser.common

import java.awt.image.BufferedImage
import java.awt.geom.{Point2D, Ellipse2D}
import java.awt.LinearGradientPaint

object TextureUtils {
  def punchedSheet(s:Int):BufferedImage = {
    val config = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
    val image = config.createCompatibleImage(s, s, java.awt.Transparency.TRANSLUCENT)
    val g2 = image.createGraphics()
        
    g2.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setRenderingHint(java.awt.RenderingHints.KEY_RENDERING, java.awt.RenderingHints.VALUE_RENDER_QUALITY)
    g2.setRenderingHint(java.awt.RenderingHints.KEY_DITHERING, java.awt.RenderingHints.VALUE_DITHER_ENABLE)
    g2.setRenderingHint(java.awt.RenderingHints.KEY_ALPHA_INTERPOLATION, java.awt.RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
    g2.setRenderingHint(java.awt.RenderingHints.KEY_COLOR_RENDERING, java.awt.RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    g2.setRenderingHint(java.awt.RenderingHints.KEY_STROKE_CONTROL, java.awt.RenderingHints.VALUE_STROKE_PURE)

    val back = new java.awt.geom.Rectangle2D.Double(0.0, 0.0, s, s)
    g2.setColor(new java.awt.Color(0x1D2123))
    g2.fill(back)

    val dark = new java.awt.Color(0x050506)
    val fractions = Array(0.0f, 1.0f)
    val colours = Array(new java.awt.Color(0, 0, 0, 255), new java.awt.Color(68, 68, 68, 255))

    val ulb = new Ellipse2D.Double(s * 0.0, s * 0.06666667014360428, s * 0.4000000059604645, s * 0.4000000059604645)
    val ulbStart = new Point2D.Double(0, ulb.getBounds2D.getMinY)
    val ulbStop = new Point2D.Double(0, ulb.getBounds2D.getMaxY)
    val ulbGradient = new LinearGradientPaint(ulbStart, ulbStop, fractions, colours)
    g2.setPaint(ulbGradient)
    g2.fill(ulb)

    val ulf = new Ellipse2D.Double(s * 0.0, s * 0.0, s * 0.4000000059604645, s * 0.4000000059604645)
    g2.setColor(dark)
    g2.fill(ulf)

    val lrb = new Ellipse2D.Double(s * 0.46666666865348816, s * 0.5333333611488342, s * 0.4000000059604645, s * 0.3999999761581421)
    val lrbStart = new Point2D.Double(0, lrb.getBounds2D.getMinY)
    val lrbStop = new Point2D.Double(0, lrb.getBounds2D.getMaxY)
    val lrbGradient = new LinearGradientPaint(lrbStart, lrbStop, fractions, colours)
    g2.setPaint(lrbGradient)
    g2.fill(lrb)

    val lrf = new Ellipse2D.Double(s * 0.46666666865348816, s * 0.46666666865348816, s * 0.4000000059604645, s * 0.4000000059604645)
    g2.setColor(dark)
    g2.fill(lrf)
    
    g2.dispose()
    
    image
  }
}

object Textures {
  lazy val PunchedSheetTexture = TextureUtils.punchedSheet(12)
}