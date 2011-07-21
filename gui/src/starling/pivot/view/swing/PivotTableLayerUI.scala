package starling.pivot.view.swing


import org.jdesktop.jxlayer.JXLayer
import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import javax.swing._
import java.awt.event.{MouseEvent, InputEvent}
import java.awt._
import java.awt.image.BufferedImage
import java.beans.PropertyChangeEvent
import org.jdesktop.animation.timing.{TimingTargetAdapter, Animator}
import starling.pivot.ColumnStructure

class PivotTableLayerUI extends AbstractLayerUI[JComponent] with DragInfo {
  object Mode extends Enumeration {
    type Mode = Value
    val Over = Value("Over Mode")
    val Drag = Value("Drag Mode")
    val Normal = Value("Normal Mode")
  }

  private var jxLayer:JXLayer[_ <: JComponent] = null
  def setJXLayer(jxLayer:JXLayer[_ <: JComponent]) = this.jxLayer = jxLayer

  import Mode._

  private var draw: Boolean = false
  private var currentField: GuiFieldComponent = null
  private var mode = Normal

  def getDraggedField: GuiFieldComponent = currentField

  def getSource: FieldChooser = {
    return currentSourcePanel
  }

  private var destinationPanel: FieldChooser = null

  private def setImageLocation(imageLocation:Point): Unit = {
    if (overImage != null) {
      resetOverImage
      mode = Drag
    }
    if (!imageLocation.equals(this.imageLocation)) {
      // Figure out what needs to be repainted.
      val oldValue = this.imageLocation
      this.imageLocation = imageLocation
      firePropertyChange("repaintClip", if (oldValue != null) oldValue else imageLocation, imageLocation)
    }
  }

  private def setResetDraggedFieldLocation(location:Point) {
    if (location != resetDraggedLocation) {
      // Figure out what needs to be repainted.
      val oldValue = resetDraggedLocation
      resetDraggedLocation = location
      firePropertyChange("repaintClipReset", if (oldValue != null) oldValue else resetDraggedLocation, resetDraggedLocation)
    }
  }

  override def handlePropertyChangeEvent(e: PropertyChangeEvent, l: JXLayer[_ <: JComponent]) = {
    if ("repaintClip" == e.getPropertyName) {
      val oldLocation = e.getOldValue.asInstanceOf[Point]
      val newLocation = e.getNewValue.asInstanceOf[Point]

      val minLocationX = math.min(oldLocation.x, newLocation.x)
      val maxLocationX = math.max(oldLocation.x, newLocation.x)
      val minLocationY = math.min(oldLocation.y, newLocation.y)
      val maxLocationY = math.max(oldLocation.y, newLocation.y)

      val minX = math.max(minLocationX - offSet.x, 0)
      val minY = math.max(minLocationY - offSet.y - imageRiseYOffset, 0)
      val clip = new Rectangle(minX, minY, maxLocationX - offSet.x + image.getWidth - minX, maxLocationY - offSet.y - imageRiseYOffset + image.getHeight - minY)

      l.repaint(clip)
    } else if ("repaintClipReset" == e.getPropertyName) {
      val oldLocation = e.getOldValue.asInstanceOf[Point]
      val newLocation = e.getNewValue.asInstanceOf[Point]

      val minLocationX = math.min(oldLocation.x, newLocation.x)
      val maxLocationX = math.max(oldLocation.x, newLocation.x)
      val minLocationY = math.min(oldLocation.y, newLocation.y)
      val maxLocationY = math.max(oldLocation.y, newLocation.y)

      val minX = math.max(minLocationX - resetOffSet.x, 0)
      val minY = math.max(minLocationY - resetOffSet.y - imageRiseYOffset, 0)
      val clip = new Rectangle(minX, minY, maxLocationX - resetOffSet.x + resetDraggedField.getWidth - minX, maxLocationY - resetOffSet.y - imageRiseYOffset + resetDraggedField.getHeight - minY)

      l.repaint(clip)
    } else if ("repaintClipSpecified" == e.getPropertyName) {
      val newLocation = e.getNewValue.asInstanceOf[Point]
      val clip = new Rectangle(newLocation.x, newLocation.y - imageRiseYOffset, overImage.getWidth, overImage.getHeight)
      l.repaint(clip)
    } else if ("clear" == e.getPropertyName) {
      val newLocation = e.getNewValue.asInstanceOf[Tuple3[Point, Int, Int]]
      val clip = new Rectangle(newLocation._1.x, newLocation._1.y - imageRiseYOffset, newLocation._2, newLocation._3)
      l.repaint(clip)
    } else {
      super.handlePropertyChangeEvent(e, l)
    }
  }

  private var imageLocation: Point = null
  private val imageRiseYOffset = 3

  def isBeingDragged: Boolean = {
    return draw
  }

  private var offSet: Point = null
  private var image: BufferedImage = null
  private var overImage:BufferedImage = null
  private var overImageLocation:Point = null
  private var initialLocation:Point = null

  def getDestination: FieldChooser = {
    return destinationPanel
  }

  private var drawResetDraggedField = false
  private var resetDraggedField:BufferedImage = null
  private var resetDraggedLocation:Point = null
  private var resetOffSet:Point = null
  private var animator:Animator = null

  protected override def processMouseEvent(e: MouseEvent, l: JXLayer[_ <: JComponent]): Unit = {
    val id: Int = e.getID
    if (draw && (id == MouseEvent.MOUSE_RELEASED)) {
      if ((getDestination == null) && (imageLocation != null)) {
        if (animator != null) {
          animator.stop
        }
        drawResetDraggedField = true
        resetDraggedField = image
        resetOffSet = offSet
        val startLocation = imageLocation
        setResetDraggedFieldLocation(startLocation)
        val widthToTravel = startLocation.x - initialLocation.x - offSet.x
        val heightToTravel = startLocation.y - initialLocation.y - offSet.y

        animator = new Animator(150)
        animator.setAcceleration(0.2f)
        animator.setDeceleration(0.2f)
        val timingTarget = new TimingTargetAdapter {
          override def timingEvent(fraction:Float) = {
            val newLocation = new Point(math.round(startLocation.x - (widthToTravel * fraction)), math.round(startLocation.y - (heightToTravel * fraction)))
            setResetDraggedFieldLocation(newLocation)
          }
          override def end = {
            drawResetDraggedField = false
            resetDraggedField = null
            resetDraggedLocation = null
            resetOffSet = null
            setDirty
          }
        }
        animator.addTarget(timingTarget)
        animator.start
      }
      reset
    }
  }

  def setOverImage(overImage:BufferedImage, overImageLocation:Point) {
    this.overImage = overImage
    this.overImageLocation = overImageLocation    
    mode = Over
    SwingUtilities.convertPointFromScreen(this.overImageLocation, jxLayer)
    firePropertyChange("repaintClipSpecified", null, this.overImageLocation)
  }

  private var overImageSource:Option[GuiFieldNamePanel] = None

  def setCurrentOverImageSource(guiField:GuiFieldNamePanel) {
    overImageSource = Some(guiField)
  }

  def resetOverImageSource {
    overImageSource = None
  }

  def getOverImageSource = overImageSource

  def resetOverImage {
    if ((mode != Normal) && (overImage != null)) {
      mode = Normal
      firePropertyChange("clear", null, (overImageLocation, overImage.getWidth, overImage.getHeight))
      overImage = null
      overImageLocation = null
    }
  }

  protected override def paintLayer(g2: Graphics2D, l: JXLayer[_ <: JComponent]): Unit = {
    super.paintLayer(g2, l)
    mode match {
      case Drag => {
        if (draw && (image != null) && (imageLocation != null)) {
          g2.setComposite(AlphaComposite.SrcOver.derive(0.6f))
          g2.drawImage(image, imageLocation.x - offSet.x, imageLocation.y - offSet.y - imageRiseYOffset, image.getWidth, image.getHeight, null)
        }
      }
      case Over => {
        val x = overImageLocation.x
        val y = overImageLocation.y
        g2.drawImage(overImage, x, y - imageRiseYOffset, overImage.getWidth, overImage.getHeight, null)
      }
      case Normal =>
    }
    if (drawResetDraggedField) {
      g2.setComposite(AlphaComposite.SrcOver.derive(0.6f))
      g2.drawImage(resetDraggedField, resetDraggedLocation.x - resetOffSet.x, resetDraggedLocation.y - resetOffSet.y - imageRiseYOffset, resetDraggedField.getWidth, resetDraggedField.getHeight, null)
    }
  }

  def setDirty: Unit = {
    setDirty(true)
  }

  private var currentSourcePanel: FieldChooser = null
  private var lastMousePosition = new Point(-1, -1)

  protected override def processMouseMotionEvent(e: MouseEvent, l: JXLayer[_ <: JComponent]): Unit = {
    try {
      val onmask: Int = InputEvent.BUTTON1_DOWN_MASK
      if (draw && (e.getModifiersEx & onmask) == onmask) {
        val position: Point = l.getMousePosition
        if (position != null) {
          setImageLocation(position)
        }
      }
    } catch {
      case e:NullPointerException => // There is an error in swing when the mouse goes off screen on a strange linux desktop setup a null pointer is thrown.
    }
  }

  def setDestination(destinationPanel: FieldChooser): Unit = {
    this.destinationPanel = destinationPanel
  }

  private var position = -1
  def setPosition(position: Int) = this.position = position
  def getPosition = position

  private var colData:ColumnStructure = null
  def getColumnData = colData
  def setColumnData(data:ColumnStructure) = colData = data

  import starling.pivot.view.swing.ColumnDragAction._
  private var colDragAction = InValid
  def getColumnDragAction = colDragAction
  def setColumnDragAction(action:ColumnDragAction) = colDragAction = action

  def initiateImage(image:BufferedImage, offSet:Point, field:GuiFieldComponent, sourcePanel:FieldChooser, initialLocation:Point):Unit = {
    if (animator != null) {
      animator.stop
    }
    this.image = image
    SwingUtilities.convertPointFromScreen(initialLocation, jxLayer)
    this.initialLocation = initialLocation
    this.offSet = offSet
    draw = true
    currentField = field
    currentSourcePanel = sourcePanel
    mode = Drag
  }

  def reset: Unit = {
    mode = Normal
    draw = false
    currentField.namePanel.reset
    currentField = null
    currentSourcePanel = null
    image = null
    imageLocation = null
    initialLocation = null
    destinationPanel = null
    setDirty
  }
}

