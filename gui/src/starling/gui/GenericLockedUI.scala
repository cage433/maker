package starling.gui

import javax.swing.JComponent
import org.jdesktop.jxlayer.plaf.ext.LockableUI
import org.jdesktop.jxlayer.JXLayer
import java.awt.{MultipleGradientPaint, Color, RadialGradientPaint, Graphics2D}
import java.awt.geom.{AffineTransform, Point2D}
import java.awt.image.BufferedImage
import org.jdesktop.animation.timing.{TimingTargetAdapter, Animator}

class GenericLockedUI extends LockableUI {
  private val defaultClient = new DefaultGenericLockedUIClient(this)
  private var client0:GenericLockedUIClient = defaultClient
  def resetDefault = client0 = defaultClient
  def setClient(client:GenericLockedUIClient) = this.client0 = client
  def client = client0

  def superSetDirty(dirty : Boolean) = super.setDirty(dirty)
  def superSetLocked(locked:Boolean) = super.setLocked(locked)
  def superPaintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = super.paintLayer(g2, l)

  override def setDirty(dirty:Boolean) = client0.setDirty(dirty)
  override def setLocked(locked:Boolean) = client0.setLocked(locked)
  override def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = client0.paintLayer(g2, l)
}

trait GenericLockedUIClient {
  def setDirty(dirty:Boolean)
  def setLocked(locked:Boolean)
  def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent])
}

class DefaultGenericLockedUIClient(genericLockedUI:GenericLockedUI) extends GenericLockedUIClient {
  def setDirty(dirty:Boolean) = genericLockedUI.superSetDirty(dirty)
  def setLocked(locked:Boolean) = {
    genericLockedUI.superSetLocked(locked)
    setDirty(true)
  }
  def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = genericLockedUI.superPaintLayer(g2, l)
}

class GreyedLockedUIClient(genericLockedUI:GenericLockedUI) extends GenericLockedUIClient {
  private var fr = 1.0f
  def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = {
    genericLockedUI.superPaintLayer(g2, l)
    if (genericLockedUI.isLocked) {
      val width = l.getWidth
      val height = l.getHeight
      val origPaint = g2.getPaint
      g2.setPaint(new RadialGradientPaint(new Point2D.Double(width / 2, width / 2), width * 0.75f,
        new Point2D.Double(width / 2, width / 2), Array(0.0f, 0.7f, 1.0f),
        Array(new Color(0, 0, 0, math.round(128 * fr)), new Color(0, 0, 0, math.round(64 * fr)), GuiUtils.ClearColour),
        MultipleGradientPaint.CycleMethod.NO_CYCLE, MultipleGradientPaint.ColorSpaceType.SRGB,
        AffineTransform.getScaleInstance(1.0, height.asInstanceOf[Double] / width.asInstanceOf[Double])))
      g2.fillRect(0, 0, width, height)
      g2.setPaint(origPaint)
    }
  }
  def setLocked(locked: Boolean) = {
    genericLockedUI.superSetLocked(locked)
    setDirty(true)
  }
  def setDirty(dirty: Boolean) = genericLockedUI.superSetDirty(dirty)
  def startDisplay {
    fr = 0.00001f
    val animator = new Animator(250)
    animator.setAcceleration(0.2f)
    animator.setDeceleration(0.2f)
    val timingTarget = new TimingTargetAdapter {
      override def timingEvent(fraction: Float) = {
        fr = fraction
        setDirty(true)
      }
      override def end = {fr = 1.0f}
    }
    animator.addTarget(timingTarget)
    animator.start
  }
}

class SlideUIClient(genericLockedUI:GenericLockedUI, direction:Int) extends GenericLockedUIClient {
  private var paintSuper = false
  def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]) = {
    if (image1 != null) {
      if (paintSuper) {
        genericLockedUI.superPaintLayer(g2, l)
        paintSuper = false
      }
      g2.drawImage(image1, null, math.round(x), 0)
      g2.drawImage(image2, null, math.round(x + image2.getWidth * direction), 0)
    } else {
      genericLockedUI.superPaintLayer(g2, l)
    }
  }
  def setLocked(locked: Boolean) = genericLockedUI.superSetLocked(locked)
  def setDirty(dirty: Boolean) = genericLockedUI.superSetDirty(dirty)
  protected var image2: BufferedImage = null
  protected var image1: BufferedImage = null
  protected var x: Float = 0.0f

  def reset {
    x = 0.0f
    image1 = null
    image2 = null
    setDirty(true)
  }

  def setImages(image1: BufferedImage, image2: BufferedImage) {
    this.image1 = image1
    this.image2 = image2
    paintSuper = true
    setDirty(true)
  }

  def setX(x: Float): Unit = {
    this.x = x
    setDirty(true)
  }
}
class BackSlideClient(genericLockedUI:GenericLockedUI) extends SlideUIClient(genericLockedUI, -1)
class ForwardSlideClient(genericLockedUI:GenericLockedUI) extends SlideUIClient(genericLockedUI, 1)

class ImageClient(genericLockedUI:GenericLockedUI) extends GenericLockedUIClient {
  def setLocked(locked:Boolean) = genericLockedUI.superSetLocked(locked)
  def setDirty(dirty:Boolean) = genericLockedUI.superSetDirty(dirty)

  private var image0:BufferedImage = null

  def image = image0
  def image_=(im:BufferedImage) {image0=im}

  def paintLayer(g2:Graphics2D, l:JXLayer[_ <: JComponent]) = {
    if (image0 != null) {
      g2.drawImage(image0,0,0,image0.getWidth(),image0.getHeight(),null)
    }
  }

  def reset {
    image0 = null    
    setDirty(true)
  }
}

class SolidColourClient(genericLockedUI:GenericLockedUI) extends GenericLockedUIClient {
  def setLocked(locked:Boolean) = genericLockedUI.superSetLocked(locked)
  def setDirty(dirty:Boolean) = genericLockedUI.superSetDirty(dirty)
  def paintLayer(g2:Graphics2D, l:JXLayer[_ <: JComponent]) = {
    g2.setColor(Color.RED)
    g2.drawRect(0,0,l.getWidth, l.getHeight)
  }
}