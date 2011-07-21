package starling.pivot.view.swing

import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import javax.swing.JComponent
import org.jdesktop.jxlayer.JXLayer
import java.awt.image.BufferedImage
import java.beans.PropertyChangeEvent
import java.awt.{Rectangle, Point, AlphaComposite, Graphics2D}
import org.jdesktop.animation.timing.{TimingTargetAdapter, Animator}

object PivotTableViewUI {
  val NullPoint = new Point(-999999, -999999)
}

import starling.pivot.view.swing.PivotTableViewUI._

object ImageType extends Enumeration {
  type ImageType = Value
  val Animation, Standard = Value
}

import ImageType._

class PivotTableViewUI extends AbstractLayerUI[JComponent] {

  private var image:BufferedImage = null
  private var imagePosition:Point = NullPoint
  private var alpha = 1.0f
  private var animator:Animator = _

  def setImageProperties(image0:BufferedImage, imagePosition0:Point, alpha0:Float, imageType:ImageType=Standard) {
    if (imageType == Standard) {
      resetAnimator()
    }
    if (image != null) {
      if (image.getWidth != image0.getWidth || image.getHeight != image0.getHeight) {
        firePropertyChange("clear", null, (imagePosition, image.getWidth, image.getHeight))
      }
    }
    image = image0
    alpha = alpha0
    if (imagePosition0 != imagePosition) {
      val oldValue = imagePosition
      imagePosition = imagePosition0
      firePropertyChange("repaintClip", oldValue, imagePosition)
    }
  }

  private def resetAnimator() {
    if (animator != null) {
      animator.stop()
      animator = null
    }
  }

  def animate(image0:BufferedImage, startPosition:Point, widthAndHeight:Point) {
    resetAnimator()
    animator = new Animator(150, new TimingTargetAdapter {
      override def timingEvent(fraction:Float) {
        val x = (startPosition.x + widthAndHeight.x * (1.0f - fraction)).round.toInt
        val y = (startPosition.y + widthAndHeight.y * (1.0f - fraction)).round.toInt
        setImageProperties(image0, new Point(x,y), 0.6f, Animation)
      }

      override def end() {resetImageProperties()}
    })
    animator.setAcceleration(0.2f)
    animator.start()
  }

  def resetImageProperties() {
    if (image != null) {
      firePropertyChange("clear", null, (imagePosition, image.getWidth, image.getHeight))
      image = null
      imagePosition = NullPoint
      resetAnimator()
    }
  }

  override def handlePropertyChangeEvent(e:PropertyChangeEvent, l:JXLayer[_ <: JComponent]) {
    if ("repaintClip" == e.getPropertyName) {
      val oldLocation = e.getOldValue.asInstanceOf[Point]
      val newLocation = e.getNewValue.asInstanceOf[Point]

      val minLocationX = math.min(oldLocation.x, newLocation.x)
      val maxLocationX = math.max(oldLocation.x, newLocation.x)
      val minLocationY = math.min(oldLocation.y, newLocation.y)
      val maxLocationY = math.max(oldLocation.y, newLocation.y)

      val minX = math.max(minLocationX, 0)
      val minY = math.max(minLocationY, 0)
      val clip = new Rectangle(minX, minY, maxLocationX + image.getWidth - minX, maxLocationY + image.getHeight - minY)

      l.repaint(clip)
    } else if ("clear" == e.getPropertyName) {
      val (point, width, height) = e.getNewValue.asInstanceOf[(Point, Int, Int)]
      val clip = new Rectangle(point.x, point.y, width, height)
      l.repaint(clip)
    } else {
      super.handlePropertyChangeEvent(e, l)
    }
  }

  override def paintLayer(g2:Graphics2D, l:JXLayer[_ <: JComponent]) {
    super.paintLayer(g2, l)
    if (image != null && imagePosition != null) {
      g2.setComposite(AlphaComposite.SrcOver.derive(alpha))
      g2.drawImage(image, imagePosition.x, imagePosition.y, image.getWidth, image.getHeight, null)
    }
  }
}