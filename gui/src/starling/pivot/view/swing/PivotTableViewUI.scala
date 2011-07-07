package starling.pivot.view.swing

import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import javax.swing.JComponent
import org.jdesktop.jxlayer.JXLayer
import java.awt.image.BufferedImage
import java.beans.PropertyChangeEvent
import java.awt.{Rectangle, Point, AlphaComposite, Graphics2D}

object PivotTableViewUI {
  val NullPoint = new Point(-999999, -999999)
}

import starling.pivot.view.swing.PivotTableViewUI._

class PivotTableViewUI extends AbstractLayerUI[JComponent] {

  private var image:BufferedImage = null
  private var imagePosition:Point = NullPoint
  private var alpha = 1.0f

  def setImageProperties(image0:BufferedImage, imagePosition0:Point, alpha0:Float) {
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

  def resetImageProperties() {
    if (image != null) {
      firePropertyChange("clear", null, (imagePosition, image.getWidth, image.getHeight))
      image = null
      imagePosition = NullPoint
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