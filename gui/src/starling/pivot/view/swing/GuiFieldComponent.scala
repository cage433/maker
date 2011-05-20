package starling.pivot.view.swing

import starling.pivot.FieldChooserType._
import swing.Swing._
import java.awt.image.BufferedImage
import java.awt.{GradientPaint, Graphics2D, Dimension, Color, RenderingHints, Point, KeyboardFocusManager}
import java.awt.event.{ComponentEvent, ComponentAdapter}
import org.jdesktop.swingx.image.ColorTintFilter
import starling.gui.GuiUtils._
import starling.gui.StarlingIcons
import starling.pivot.model.TreeDetails
import org.jdesktop.swingx.graphics.ShadowRenderer
import swing._
import swing.event._
import starling.pivot.controller.{TreePivotFilterNode, TreePivotFilter}
import javax.swing.event.{PopupMenuListener, PopupMenuEvent}
import javax.swing.{JPopupMenu, SwingUtilities}
import starling.gui.custom._
import starling.pivot._

case class GuiFieldComponentProps(field:Field, locationOfField:FieldChooserType,
                                  showDepthPanel:Boolean, measureField:Boolean, realMeasureField:Boolean,
                                  treeDetails:TreeDetails, onDepthChange:((Field, (Int, Int)) => Unit),
                                  onMeasureChange:(Field, FieldChooserType) => Unit,
                                  filterData:FilterData, transformData:TransformData,
                                  otherLayoutInfo:OtherLayoutInfo, onSubTotalToggle:(Field, FieldChooserType) => Unit,
                                  showSubTotalToggle:Boolean, viewUI:PivotTableViewUI, tableView:PivotTableView)

case class FilterData(possibleValuesAndSelection:Option[(TreePivotFilter, Selection)], onFilterChange:((Field, Selection) => Unit))
case class TransformData(showOther:Boolean, transforms:Option[FilterWithOtherTransform], onTransformChange:((Field,FilterWithOtherTransform) => Unit))

object GuiFieldComponent {
  val HorizontalGap = 15 // The gap at the sides that dictates when the mouse is in the left/right section of the gui field.
  val VerticalGap = 6 // The gap at the bottom that dictates when the mouse is in the top/bottom section of the gui field, when it is in the left/right section.

  val MeasureWidth = 8

  def drawMeasure(g:Graphics2D, h:Int, sx0:Int) {
    val sy = 5
    val ey = h - (sy + 1)
    val sx = sx0
    val sx1 = sx + 1

    val ex1 = sx + 5
    val ex2 = sx + 3
    val ex3 = sx + 1

    // Left side or ruler
    g.drawLine(sx, sy, sx, ey)

    // The first x line.
    g.drawLine(sx1, sy, ex1, sy)

    // Other bits
    for (y <- sy to ey) {
      val pos = (y - sy) % 17
      pos match {
        case 16 => g.drawLine(sx1, y, ex1, y)
        case 2 | 4 | 6 | 10 | 12 | 14 => g.drawLine(sx1, y, ex3, y)
        case 8 => g.drawLine(sx1, y, ex2, y)
        case _ =>
      }
    }
  }
}

import GuiFieldComponent._

case class GuiFieldComponent(props:GuiFieldComponentProps) extends MigPanel("insets 0, hidemode 3", "[p]0[p]0[p]0[p]0[p]0[p]") {
  opaque = false

  val namePanel = GuiFieldNamePanel(props, this)
  val treeLevelPanel = TreeLevelPanel(props)
  val measureTogglePanel = MeasureTogglePanel(props)
  val subTotalTogglePanel = SubTotalTogglePanel(props)
  val filterLabelPanel = FilterLabelPanel(props)
  val possibleValuesAndSelectionToUse = getPossibleValuesAndSelection

  private def getPossibleValuesAndSelection = props.filterData.possibleValuesAndSelection match {
    case None => (TreePivotFilter(TreePivotFilterNode("All", "All", List())), AllSelection)
    case Some(d) => d
  }

  private val transformData = props.transformData

  val filterPopupPanel = TreePanel(possibleValuesAndSelectionToUse, transformData.showOther, transformData.transforms)

  val popupMenu = new JPopupMenu {
    add(filterPopupPanel.peer)
    addPopupMenuListener(new PopupMenuListener {
      def popupMenuCanceled(e:PopupMenuEvent) {}
      def popupMenuWillBecomeInvisible(e:PopupMenuEvent) {
        // Whenever the popup panel is hidden, ensure it represents the state of the page.
        filterPopupPanel.filterPanel.textField.text = ""
        filterPopupPanel.filterHelper.resetPopup(getPossibleValuesAndSelection, props.transformData.transforms)
      }
      def popupMenuWillBecomeVisible(e:PopupMenuEvent) {}
    })
  }
  val filterButtonPanel = FilterButtonPanel(props)

  val (values, selection) = possibleValuesAndSelectionToUse
  val (textToUse, numberTextToUse) = selection match {
    case AllSelection => {(filterPopupPanel.treeComponent.rootNode.getUserObject.asInstanceOf[CheckBoxListElement].label,"")}
    case SomeSelection(selectedValues) if selectedValues.isEmpty => {(TreePanelComboBox.NONE.toString, "0")}
    case SomeSelection(selectedValues) => {
      // Need to use the label
      (selectedValues.toList.map(v => {
        val s = filterPopupPanel.filterHelper.valueToLabelMap.getOrElse(v, "Unknown:" + v)
        if (s.length == 0) " " else s
      }).mkString(","), selectedValues.size.toString)
    }
  }
  filterLabelPanel.label.text = textToUse
  filterButtonPanel.numberText = numberTextToUse

  reactions += {
    case DisplayPopupEvent(`filterButtonPanel`) => {
      // Find out where to display the popup.
      if (filterPopupPanel.preferredSize.width < 200) {
        filterPopupPanel.preferredSize = new Dimension(200, filterPopupPanel.preferredSize.height)
      }
      val yPos = filterButtonPanel.size.height - 1
      val xPos = filterButtonPanel.size.width - filterPopupPanel.preferredSize.width - 7
      filterPopupPanel.scrollToFirstSelectedNode
      popupMenu.show(filterButtonPanel.peer, xPos, yPos)
      onEDT({
        KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
        filterPopupPanel.filterPanel.textField.requestFocusInWindow()
      })
    }
    case FilterSelectionChanged(`filterPopupPanel`, sel) => props.filterData.onFilterChange(props.field, sel)
    case OtherValueSelectionChanged(`filterPopupPanel`, sel) => {
      // In all cases sel should be some selection.
      sel match {
        case SomeSelection(s) => props.transformData.onTransformChange(props.field, FilterWithOtherTransform(s))
        case _ => throw new Exception("This should never happen")
      }
    }
    case CancelEvent(`filterPopupPanel`) => popupMenu.setVisible(false)
  }
  listenTo(filterButtonPanel, filterPopupPanel)

  // If the tree level panel is available, we want to grow this so the buttons are on the left rather than growing the name panel and putting the
  // buttons on the right.
  if (!treeLevelPanel.visible) {
    add(namePanel, "push, grow")
    add(treeLevelPanel, "grow")
  } else {
    add(namePanel, "grow")
    add(treeLevelPanel, "push, grow")
  }
  add(measureTogglePanel, "grow")
  add(subTotalTogglePanel, "grow")
  add(filterLabelPanel, "grow")
  add(filterButtonPanel, "grow")

  def setPreferredWidth(width:Int) {
    val sizeToUse = new Dimension(width, preferredSize.height)
    preferredSize = sizeToUse
    peer.setSize(sizeToUse)
    minimumSize = sizeToUse
    maximumSize = sizeToUse
    namePanel.setMaxSizeForLabel(sizeToUse)
  }

  val initialPreferredSize = preferredSize
}

case class TempGuiFieldNamePanel(fieldName:String) extends MigPanel {
  val label = new Label(fieldName) {
    font = GuiFieldFont
  }
  add(label, "pushx,growx")
  minimumSize = new Dimension(math.max(preferredSize.width, PivotJTable.MinColumnWidth), preferredSize.height)

  override protected def paintComponent(g:Graphics2D) = {
    val width = size.width
    val height = size.height

    val im = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2 = im.getGraphics.asInstanceOf[Graphics2D]

    val x = 1
    val y = 1
    val fy = y + 1
    val w = width - 3
    val fw = w + 1
    val h = height - 3
    val fh = h - 1

    val gradPaint = new GradientPaint(x, y, GUIFieldTopColour, x, h, GUIFieldBottomColour)
    val oldPaint = g2.getPaint
    g2.setPaint(gradPaint)

    g2.fillRoundRect(x, fy, fw, fh, GuiFieldArc - 2, GuiFieldArc - 2)
    g2.setPaint(oldPaint)

    val oldColour = g2.getColor
    g2.setColor(GuiFieldBorderColour)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.drawRoundRect(x, y, w, h, GuiFieldArc, GuiFieldArc)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    g2.setColor(oldColour)

    g2.dispose()

    val colorTintFilter = new ColorTintFilter(Color.BLUE, 0.1f)

    g.drawImage(colorTintFilter.filter(im,null), 0, 0, null)
  }
}

case class GuiFieldNamePanel(props:GuiFieldComponentProps, guiComp:GuiFieldComponent) extends MigPanel {
  opaque = false
  background = ClearColour

  private var image:BufferedImage = null
  private var tintedImage:BufferedImage = null
  private val bothEndsRounded = ((props.locationOfField == FieldList) ||
          (props.realMeasureField && (props.locationOfField != Filter && !props.showSubTotalToggle)))
  private val connectedTo = !bothEndsRounded
  private val drawMeasureMark = props.realMeasureField
  private var display = true

  private var dragging = false

  // As I'm drawing a measure symbol on real measure fields, I need to give a little bit more space.
  private val nameToUse = if (drawMeasureMark) {
    props.field.name + "  "
  } else {
    props.field.name
  }

  private val label = new Label(nameToUse) {
    font = GuiFieldFont
    visible = false
  }
  add(label, "pushx,growx")

  minimumSize = new Dimension(math.max(preferredSize.width, PivotJTable.MinColumnWidth), preferredSize.height)

  def setDisplay(display:Boolean) {this.display = display}

  def setDragging(dragging:Boolean) {this.dragging = dragging}

  def getImage:BufferedImage = if (image != null) image else createMainImage

  def setMaxSizeForLabel(size:Dimension) {label.maximumSize = new Dimension(size.width - 10, size.height)}

  private val arc = GuiFieldArc

  private def createMainImage = {
    val width = size.width
    val height = size.height
    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2 = image.getGraphics.asInstanceOf[Graphics2D]

    val x = 1
    val y = 1
    val fy = y + 1
    val w = if (connectedTo) width - 2 else width - 3
    val fw = w + 1
    val h = height - 3
    val fh = h - 1

    val bottomColour = if (props.measureField || props.realMeasureField) {
      MeasureGUIFieldBottomColour
    } else {
      GUIFieldBottomColour
    }

    val gradPaint = new GradientPaint(x, y, GUIFieldTopColour, x, h, bottomColour)
    val oldPaint = g2.getPaint
    g2.setPaint(gradPaint)

    if (bothEndsRounded) {
      //    g2.fillRect(x, y, w, h)
      g2.fillRoundRect(x, fy, fw, fh, arc - 2, arc - 2)
      g2.setPaint(oldPaint)

      val oldColour = g2.getColor
      g2.setColor(GuiFieldBorderColour)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.drawRoundRect(x, y, w, h, arc, arc)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g2.setColor(oldColour)
    } else {
      g2.fillRoundRect(x, fy, arc, fh, arc - 2, arc - 2)
      g2.setPaint(oldPaint)

      val oldColour = g2.getColor
      g2.setColor(GuiFieldBorderColour)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.drawRoundRect(x, y, arc, h, arc, arc)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
      g2.setColor(oldColour)

      val arcBy2 = arc / 2
      g2.setPaint(gradPaint)
      g2.fillRect(x + arcBy2, fy, fw - arcBy2, fh)
      g2.setPaint(oldPaint)

      g2.setColor(GuiFieldBorderColour)
      g2.drawLine(arc - 2, y, x + w, y)
      g2.drawLine(x + w, y, x + w, y + h)
      g2.drawLine(arc - 2, y + h, x + w, y + h)
      g2.setColor(oldColour)
    }

    label.visible = true
    super.paintChildren(g2)
    label.visible = false

    if (drawMeasureMark) {
      g2.setColor(Color.GRAY)
      val sx = width - MeasureWidth - 3
      drawMeasure(g2, height, sx)
    }
    g2.dispose()
    shadowImage = generateShadowImage
    image
  }

  // TODO - change this to a UI element reaction
  peer.addComponentListener(new ComponentAdapter {
    override def componentResized(e:ComponentEvent) {
      if ((image == null) || (image.getWidth != size.width) || (image.getHeight != size.height)) {
        image = null
        repaint()
      }
    }
  })

  private def generateShadowImage = {
    val shadowRenderer = new ShadowRenderer(2, 0.5f, Color.BLACK)
    val shadowImage = shadowRenderer.createShadow(getImage)
    val g = shadowImage.getGraphics
    g.drawImage(image, 0, 0, null)
    g.dispose()
    shadowImage
  }

  private var shadowImage:BufferedImage = null
  private var offSet = PivotTableViewUI.NullPoint

  reactions += {
    case MousePressed(_,p,_,_,_) => {
      offSet = p
    }
    case MouseClicked(_,_,_,2,_) => {
      props.tableView.fieldDoubleClicked(props.field, props.locationOfField)
    }
    case MouseEntered(_, _, _) if !props.tableView.fieldBeingDragged => {
      display = false
      val displayPoint = SwingUtilities.convertPoint(peer, 0, -2, props.tableView.peer)
      props.viewUI.setImageProperties(shadowImage, displayPoint, 1.0f)
    }
    case MouseExited(_, _, _) if !props.tableView.fieldBeingDragged => {
      display = true
      props.viewUI.resetImageProperties()
    }
    case MouseReleased(_,p,_,_,_) => {
      if (dragging) {
        val screenPoint = new Point(p)
        SwingUtilities.convertPointToScreen(screenPoint, peer)
        props.tableView.fieldDropped(props.field, props.locationOfField, screenPoint)
        reset()
      }
    }
    case MouseDragged(_,p,_) => {
      props.tableView.draggedField = props.field
      props.tableView.fieldBeingDragged = true
      dragging = true
      val displayPoint = SwingUtilities.convertPoint(peer, p.x - offSet.x, p.y - offSet.y - 2, props.tableView.peer)
      props.viewUI.setImageProperties(shadowImage, displayPoint, 0.6f)
    }
  }
  listenTo(mouse.clicks, mouse.moves)

  override protected def paintComponent(g:Graphics2D) {
    if (display && !dragging) {
      if (image == null) {
        image = createMainImage
      }
      g.drawImage(image, 0, 0, null)
    } else if (dragging) {
      if (tintedImage == null) {
        if (image == null) {
          image = createMainImage
        }
        val colorTintFilter = new ColorTintFilter(Color.GREEN, 0.1f)
        tintedImage = colorTintFilter.filter(image, null)
      }
      g.drawImage(tintedImage, 0, 0, null)
    }
  }

  def reset() {
    dragging = false
    display = true
    props.tableView.fieldBeingDragged = false
    props.viewUI.resetImageProperties()
    repaint()
  }
}

case class BorderBuilder(var top:Boolean = true, var left:Boolean = true, var bottom:Boolean = true, var right:Boolean = true)

class GuiFieldPanel(constraints:String, isMeasure:Boolean, isSubTotalToggle:Boolean=false,
                    borderBuilder:BorderBuilder = BorderBuilder(left = false), endPiece:Boolean=false) extends MigPanel(constraints) {
  opaque = false

  def dim = {
    val x = 0
    val y = 1
    val w = size.width - (if (borderBuilder.right) 1 else 0)
    val h = size.height - 3
    (x, y, w, h)
  }
    
  private val arc = GuiFieldArc

  override protected def paintComponent(g2:Graphics2D) {
    if (!endPiece) {
      val (x, y, w, h) = dim
      val bottomColour = if (isMeasure) {
        MeasureGUIFieldBottomColour
      } else if (isSubTotalToggle) {
        SubtotalTotalColour
      } else {
        GUIFieldBottomColour
      }
      val gradPaint = new GradientPaint(x, y, GUIFieldTopColour, x, h, bottomColour)
      val oldPaint = g2.getPaint
      g2.setPaint(gradPaint)
      g2.fillRect(x, y, w, h)
      g2.setPaint(oldPaint)
      super.paintChildren(g2)
    } else {

      val x = 0
      val y = 1
      val w = size.width - 2
      val h = size.height - 3

      val fy = y + 1
      val fw = w + 1
      val fh = h - 1

      val bottomColour = if (isMeasure) {
        MeasureGUIFieldBottomColour
      } else if (isSubTotalToggle) {
        SubtotalTotalColour
      } else {
        GUIFieldBottomColour
      }

      val gradPaint = new GradientPaint(x, y, GUIFieldTopColour, x, h, bottomColour)
      g2.setPaint(gradPaint)

      g2.fillRoundRect(fw - arc, fy, arc, fh, arc - 2, arc - 2)

      g2.setColor(GuiFieldBorderColour)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.drawRoundRect(w - arc, y, arc, h, arc, arc)
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)

      val arcBy2 = arc / 2
      g2.setPaint(gradPaint)
      g2.fillRect(x, fy, fw - arcBy2 - 1, fh)

      g2.setColor(GuiFieldBorderColour)

      g2.drawLine(x, y, x + fw - arc + 1, y)
      g2.drawLine(x, y + h, x + fw - arc + 1, y + h)
    }
  }

  override protected def paintBorder(g2:Graphics2D) {
    if (!endPiece) {
      val (x, y, w, h) = dim
      g2.setColor(GuiFieldBorderColour)
      if (borderBuilder.top) {
        g2.drawLine(x, y, x + w, y)
      }
      if (borderBuilder.bottom) {
        g2.drawLine(x, y + h, x + w, y + h)
      }
      if (borderBuilder.right) {
        g2.drawLine(x + w, y, x + w, y + h)
      }
      if (borderBuilder.left) {
        g2.drawLine(x, y, x, y + h)
      }
    }
  }
}

object TreeLevelPanel {
  val LeftIcon = StarlingIcons.im("/icons/5x10_bullet-arrow-left.png")
  val DisabledLeftIcon = createDisabledBufferedImage(LeftIcon)
  val RightIcon = StarlingIcons.im("/icons/5x10_bullet-arrow-right.png")
  val DisabledRightIcon = createDisabledBufferedImage(RightIcon)
}

case class TreeLevelPanel(props:GuiFieldComponentProps) extends GuiFieldPanel("insets n 2lp n 3lp, gap 2lp", (props.measureField || props.realMeasureField)) {
  import TreeLevelPanel._
  visible = props.showDepthPanel

  val currentTreeDepth = props.treeDetails.treeDepths.getOrElse(props.field, (0, 0))
  val maxTreeDepth = props.treeDetails.maxTreeDepths.getOrElse(props.field, 0)

  val leftButton = new ImageButtonWithDisabledImageSupplied(LeftIcon, DisabledLeftIcon,
    {props.onDepthChange(props.field, (currentTreeDepth._1, currentTreeDepth._2 - 1))}) {
    enabled = ((currentTreeDepth._2) > currentTreeDepth._1)
  }
  val rightButton = new ImageButtonWithDisabledImageSupplied(RightIcon, DisabledRightIcon,
    {props.onDepthChange(props.field, (currentTreeDepth._1, currentTreeDepth._2 + 1))}) {
    enabled = (currentTreeDepth._2 < maxTreeDepth)
  }
  val depthLabel = new Label(currentTreeDepth._2.toString) {
    font = GuiFieldFont
    tooltip = "The current depth of the leaf"
  }

  val rootLeftButton = new ImageButtonWithDisabledImageSupplied(LeftIcon, DisabledLeftIcon,
    {props.onDepthChange(props.field, (currentTreeDepth._1 - 1, currentTreeDepth._2))}) {
    enabled = (currentTreeDepth._1 > 0)
  }
  val rootRightButton = new ImageButtonWithDisabledImageSupplied(RightIcon, DisabledRightIcon,
    {props.onDepthChange(props.field, (currentTreeDepth._1 + 1, currentTreeDepth._2))}) {
    enabled = ((currentTreeDepth._1) < currentTreeDepth._2)
  }
  val rootDepthLabel = new Label(currentTreeDepth._1.toString) {
    font = GuiFieldFont
    tooltip = "The current depth of the root"
  }

  add(rootLeftButton, "pushy, growy, ay center")
  add(rootDepthLabel)
  add(rootRightButton)

  add(leftButton)
  add(depthLabel)
  add(rightButton)
}

case class MeasureTogglePanel(props:GuiFieldComponentProps) extends GuiFieldPanel("insets 2 0 2 1", true) {
  visible = (!props.realMeasureField && (props.locationOfField == Columns))
  tooltip = "Toggles whether this should act as a measure field"
  minimumSize = new Dimension(13, 1)

  private var mouseOver = false
  reactions += {
    case MouseEntered(_, _, _) => mouseOver = true; repaint()
    case MouseExited(_, _, _) => mouseOver = false; repaint()
    case MouseClicked(_, _, _, _, _) => mouseOver = false; repaint(); props.onMeasureChange(props.field, props.locationOfField)
  }
  listenTo(mouse.moves, mouse.clicks)

  override protected def paintComponent(g:Graphics2D) {
    val image = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
    val imG = image.getGraphics.asInstanceOf[Graphics2D]
    super.paintComponent(imG)
    imG.dispose()

    val imageToDraw = if (mouseOver && !props.tableView.fieldBeingDragged) {
      val colorTintFilter = new ColorTintFilter(Color.GRAY, 0.1f)
      colorTintFilter.filter(image, null)
    } else {
      image
    }
    g.drawImage(imageToDraw, 0, 0, null)

    g.setColor(Color.GRAY)
    drawMeasure(g, size.height, 3)
  }
}

case class SubTotalTogglePanel(props:GuiFieldComponentProps)
        extends GuiFieldPanel("insets 2 0 2 1", false,
          !(props.otherLayoutInfo.rowSubTotalsDisabled.contains(props.field) ||
                  props.otherLayoutInfo.columnSubTotalsDisabled.contains(props.field)),
          endPiece = props.realMeasureField) {
  visible = props.showSubTotalToggle
  tooltip = "Toggle whether subtotals should be shown for this field"
  val (extraWidth, extraInfo) = if (props.realMeasureField) (1, ",gapright 1") else (0,"")
  minimumSize = new Dimension(13 + extraWidth, 1)
  val sumLabel = new Label {
    icon = StarlingIcons.icon("/icons/8x8_sum.png")
  }
  add(sumLabel, "push, al center" + extraInfo)

  private var mouseOver = false
  reactions += {
    case MouseEntered(_, _, _) => mouseOver = true; repaint()
    case MouseExited(_, _, _) => mouseOver = false; repaint()
    case MouseClicked(_, _, _, _, _) => mouseOver = false; repaint(); props.onSubTotalToggle(props.field, props.locationOfField)
  }
  listenTo(mouse.moves, mouse.clicks)

  override protected def paintComponent(g:Graphics2D) = {
    val image = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
    val imG = image.getGraphics.asInstanceOf[Graphics2D]
    super.paintComponent(imG)
    imG.dispose()

    val imageToDraw = if (mouseOver && !props.tableView.fieldBeingDragged) {
      val colorTintFilter = new ColorTintFilter(Color.GRAY, 0.1f)
      colorTintFilter.filter(image, null)
    } else {
      image
    }
    g.drawImage(imageToDraw, 0, 0, null)
  }
}

case class FilterLabelPanel(props:GuiFieldComponentProps)
        extends GuiFieldPanel("insets n n n 2lp, gap 2lp, hidemode 2",
          (props.measureField || props.realMeasureField), false,
          BorderBuilder(left = false, right = (props.locationOfField != Filter))) {
  visible = (props.locationOfField == Filter)

  val label = new Label {
    font = GuiFieldFont
    text = ""
    foreground = Color.BLUE.darker
    maximumSize = new Dimension(150, Integer.MAX_VALUE)

    override def text_=(s:String) {
      super.text = s
      tooltip = s
    }
  }

  add(label)
}

case class DisplayPopupEvent(source:Component) extends Event

case class FilterButtonPanel(props:GuiFieldComponentProps) extends MigPanel {
  visible = ((props.locationOfField == Filter || !props.realMeasureField) && (props.locationOfField != FieldList))

  opaque = false
  background = ClearColour

  private val drawLeftLineOnMouseOver = (props.locationOfField == Filter)
  private var mouseInPanel = false
  private val arc = GuiFieldArc
  var numberText = ""

  val downArrow = new FixedImagePanel(StarlingIcons.im("/icons/small_down_arrow.png"))

  add(downArrow, "al center, pushy, growy, gapright 2lp, gaptop 1lp")

  reactions += {
    case MouseClicked(_, _, _, _, _) if enabled => publish(DisplayPopupEvent(this))
    case MouseEntered(_, _, _) if enabled && !props.tableView.fieldBeingDragged => mouseInPanel = true; repaint()
    case MouseExited(_, _, _) if enabled && !props.tableView.fieldBeingDragged => mouseInPanel = false; repaint()
  }
  listenTo(mouse.moves, mouse.clicks)

  override protected def paintComponent(g2:Graphics2D) {
    val x = 0
    val y = 1
    val w = size.width - 2
    val h = size.height - 3

    val fy = y + 1
    val fw = w + 1
    val fh = h - 1

    val bottomColour = if (props.measureField || props.realMeasureField) {
      MeasureGUIFieldBottomColour
    } else {
      GUIFieldBottomColour
    }
    val (colour1ToUse, colour2ToUse) = if (!mouseInPanel) {
      (GUIFieldTopColour, bottomColour)
    } else {
      (GUIFieldTopColour, bottomColour.darker)
    }
    val gradPaint = new GradientPaint(x, y, colour1ToUse, x, h, colour2ToUse)
    g2.setPaint(gradPaint)

    g2.fillRoundRect(fw - arc, fy, arc, fh, arc - 2, arc - 2)

    g2.setColor(GuiFieldBorderColour)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.drawRoundRect(w - arc, y, arc, h, arc, arc)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)

    val arcBy2 = arc / 2
    g2.setPaint(gradPaint)
    g2.fillRect(x, fy, fw - arcBy2 - 1, fh)

    g2.setColor(GuiFieldBorderColour)

    g2.drawLine(x, y, x + fw - arc + 1, y)
    g2.drawLine(x, y + h, x + fw - arc + 1, y + h)
    if (drawLeftLineOnMouseOver && mouseInPanel) {
      g2.drawLine(x, y, x, y + h)
    }

    val lm = GuiFieldFilterNumberFont.getLineMetrics(numberText, g2.getFontRenderContext)

    // Draw the number text manually.
//    g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2.setFont(GuiFieldFilterNumberFont)
    g2.setColor(if (numberText != "0") GuiFieldFilterNumberColour else Color.RED)
    g2.drawString(numberText, 1,lm.getHeight.round)
  }

  override def enabled_=(b:Boolean) {
    super.enabled = b
    downArrow.enabled = b
  }

  enabled = !props.measureField
}