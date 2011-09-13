package starling.browser.common

import net.miginfocom.swing.MigLayout
import java.awt.image.BufferedImage
import java.awt.{Dimension, Color, Graphics2D, RenderingHints, Insets, Cursor, GradientPaint, AlphaComposite, LayoutManager}
import scala.swing.Swing.EmptyIcon
import swing._
import org.jdesktop.swingx.{JXList, JXLabel, JXPanel}
import org.jdesktop.swingx.decorator.Highlighter
import javax.swing._
import swing.event.{MouseExited, MouseEntered, MouseClicked}
import GuiUtils._
import org.jdesktop.swingx.painter.{ImagePainter, PinstripePainter, Painter}

class MigPanel(layoutConstraints:String = "", columnConstraints:String = "", rowConstraints:String = "")
        extends Panel with SequentialContainer.Wrapper {
  override lazy val peer: javax.swing.JPanel = new javax.swing.JPanel(
    new MigLayout(layoutConstraints, columnConstraints, rowConstraints)) with SuperMixin

  protected def add(component: Component, constraints: String) { peer.add(component.peer, constraints) }
  protected def add(components: Component*) { components.map(add(_, "")) }
  protected def add(component: JComponent, constraints: String) { peer.add(component, constraints) }
  protected def add(component: JComponent) { add(component, "") }

  protected def removeAll {peer.removeAll}
  protected def remove(comp:Component) {peer.remove(comp.peer)}

  def this(layoutConstraints: String) = this(layoutConstraints, "", "")
  def this(layoutConstraints: String, columnConstraints: String) = this(layoutConstraints, columnConstraints, "")
}

class MigXPanel (layoutConstraints:String="", columnConstraints:String="", rowConstraints:String="")
        extends Panel with SequentialContainer.Wrapper {
  override lazy val peer: JXPanel = new JXPanel(
    new MigLayout(layoutConstraints, columnConstraints, rowConstraints)) with SuperMixin

  protected def add(component: Component, constraints: String) { peer.add(component.peer, constraints) }
  protected def add(component: Component) { add(component, "") }
  protected def add(component: JComponent, constraints: String) { peer.add(component, constraints) }

  def backgroundPainter = peer.getBackgroundPainter
  def backgroundPainter_=(painter:Painter[_]) {peer.setBackgroundPainter(painter)}
}

class StripedPanel(layoutConstraints:String = "", columnConstraints:String = "", rowConstraints:String = "")
        extends MigXPanel(layoutConstraints, columnConstraints, rowConstraints) {
  background = GuiUtils.TaskPageBackgroundColour
  backgroundPainter = new PinstripePainter(Color.WHITE, 0.0, 0.5, 10.0)
}

class FixedImagePanel(var image0:BufferedImage) extends MigXPanel("insets 0") {
  opaque = false

  def generateGreyedImage = {
    val iSize = new Dimension(image0.getWidth, image0.getHeight)
    preferredSize =  iSize
    minimumSize = iSize
    maximumSize = iSize
    val greyedImage = GrayFilter.createDisabledImage(image0)
    val bufferedImage = new BufferedImage(iSize.width, iSize.height, BufferedImage.TYPE_INT_ARGB)
    bufferedImage.getGraphics.drawImage(greyedImage, 0, 0, null)
    bufferedImage
  }

  private var greyedImage:BufferedImage = generateGreyedImage

  def image = image0
  def image_=(im:BufferedImage) {
    image0 = im
    greyedImage = generateGreyedImage
    backgroundPainter = new ImagePainter(image0)
  }

  backgroundPainter = new ImagePainter(image0)

  override def enabled_=(b:Boolean) {
    if (b) {
      backgroundPainter = new ImagePainter(image0)
    } else {
      backgroundPainter = new ImagePainter(greyedImage)
    }
    super.enabled = b
  }
}

class TwoFixedImagePanel(image1:BufferedImage, image2:BufferedImage, clicked: => Unit) extends FixedImagePanel(image1) {
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  reactions += {
    case MouseClicked(_,_,_,_,_) => {
      clicked
      backgroundPainter = new ImagePainter(image1)
    }
    case MouseEntered(_,_,_) => {
      backgroundPainter = new ImagePainter(image2)
    }
    case MouseExited(_,_,_) => {
      backgroundPainter = new ImagePainter(image1)
    }
  }
  listenTo(mouse.clicks, mouse.moves)
}

class FixedImagePanelWithDisabledImageSupplied(image:BufferedImage,disabledImage:BufferedImage) extends MigXPanel("insets 0") {
  opaque = false
  private val iSize = new Dimension(image.getWidth, image.getHeight)
  preferredSize =  iSize
  minimumSize = iSize
  maximumSize = iSize

  backgroundPainter = new ImagePainter(image)

  override def enabled_=(b:Boolean) {
    if (b) {
      backgroundPainter = new ImagePainter(image)
    } else {
      backgroundPainter = new ImagePainter(disabledImage)
    }
    super.enabled = b
  }
}

class ImageButtonWithDisabledImageSupplied(image:BufferedImage, disabledImage:BufferedImage, action: => Unit)
        extends FixedImagePanelWithDisabledImageSupplied(image, disabledImage) {
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  reactions += {
    case MouseClicked(_,_,_,_,_) => {
      if (enabled) {
        action
      }
    }
  }
  listenTo(mouse.clicks)
}

class ImageButton(image:BufferedImage, action: => Unit) extends FixedImagePanel(image) {
  cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  reactions += {
    case MouseClicked(_,_,_,_,_) => {
      if (enabled) {
        action
      }
    }
  }
  listenTo(mouse.clicks)
}

case class ButtonClickedEx(override val source: scala.swing.AbstractButton, e:java.awt.event.ActionEvent) extends scala.swing.event.ActionEvent(source)

class VerticalButton(text:String, clockwise:Boolean = false) extends JButton {
  val f = getFont
  val fm = getFontMetrics(f)
  val captionHeight = fm.getHeight
  val captionWidth = fm.stringWidth(text)
  val bi = new BufferedImage(captionHeight + 2, captionWidth + 2, BufferedImage.TYPE_INT_ARGB)
  val g = bi.getGraphics.asInstanceOf[Graphics2D]
  g.setColor(GuiUtils.ClearColour)
  g.fillRect(0, 0, bi.getWidth, bi.getHeight)
  g.setColor(getForeground)
  g.setFont(f)
  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  if (clockwise) {
    g.rotate(java.lang.Math.PI / 2)
  }
  else {
    g.rotate(-java.lang.Math.PI / 2)
    g.translate(-bi.getHeight, bi.getWidth)
  }
  g.drawString(text, 2, -4)
  val icon = new ImageIcon(bi)
  setIcon(icon)
  setMargin(new Insets(0, 0, 0, 0))
  setActionCommand(text)
}

class SXLabel(text0: String, icon0: Icon, align: Alignment.Value) extends Component {
  override lazy val peer: JXLabel = new JXLabel(text0, toNullIcon(icon0), align.id) with SuperMixin

  def toNullIcon(i: Icon): Icon = if(i == EmptyIcon) null else i
  def this() = this("", EmptyIcon, Alignment.Center)
  def this(s: String) = this(s, EmptyIcon, Alignment.Center)
  def text: String = peer.getText
  def text_=(s: String) {peer.setText(s)}
  def icon: Icon = peer.getIcon
  def icon_=(i: Icon) {peer.setIcon(i)}

  def textRotation = peer.getTextRotation
  def textRotation_=(textOrientation:Double) {peer.setTextRotation(textOrientation)}

  /**
   * The alignment of the label's contents relative to its bounding box.
   */
  def xAlignment: Alignment.Value = Alignment(peer.getHorizontalAlignment)
  def xAlignment_=(x: Alignment.Value) { peer.setHorizontalAlignment(x.id) }
  def yAlignment: Alignment.Value = Alignment(peer.getVerticalAlignment)
  def yAlignment_=(x: Alignment.Value) { peer.setVerticalAlignment(x.id) }

  /** @see javax.swing.JLabel#getHorizontalAlignment() */
  def horizontalAlignment: Alignment.Value = Alignment(peer.getHorizontalAlignment)
  /** @see javax.swing.JLabel#setHorizontalAlignment() */
  def horizontalAlignment_=(x: Alignment.Value) { peer.setHorizontalAlignment(x.id) }

  def verticalAlignment: Alignment.Value = Alignment(peer.getVerticalAlignment)
  def verticalAlignment_=(x: Alignment.Value) { peer.setVerticalAlignment(x.id) }

  def horizontalTextPosition: Alignment.Value = Alignment(peer.getHorizontalTextPosition)
  def horizontalTextPosition_=(x: Alignment.Value) { peer.setHorizontalTextPosition(x.id) }

  def verticalTextPosition: Alignment.Value = Alignment(peer.getVerticalTextPosition)
  def verticalTextPosition_=(x: Alignment.Value) { peer.setVerticalTextPosition(x.id) }

  def disabledIcon: Icon = peer.getDisabledIcon
  def disabledIcon_=(icon: Icon) { peer.setDisabledIcon(icon) }

  def iconTextGap: Int = peer.getIconTextGap
  def iconTextGap_=(gap: Int) { peer.setIconTextGap(gap) }

  def displayedMnemonicIndex: Int = peer.getDisplayedMnemonicIndex
  def displayedMnemonicIndex_=(index: Int) { peer.setDisplayedMnemonicIndex(index) }
}

class ListXView[A] extends ListView[A] {
  override lazy val peer:JXList = new JXList(true)
  def this(items:Seq[A]) = {
    this()
    listData = items
  }

  def rollover = peer.isRolloverEnabled
  def rollover_=(b:Boolean) {peer.setRolloverEnabled(b)}
  def addHighlighter(highlighter:Highlighter) {peer.addHighlighter(highlighter)}
  def rowFilter = peer.getRowFilter
  def rowFilter_=(rowFilter:RowFilter[_ >: javax.swing.ListModel,_ >: java.lang.Integer]) {peer.setRowFilter(rowFilter)}
}

class NListView[T](values:Seq[T]) extends ListView[T](values) {
  selection.intervalMode = ListView.IntervalMode.Single

  def selectedOption:Option[T] = if (selection.indices.isEmpty) None else Some(selected)
  def selectedOption_=(v:Option[T]) {v match {
    case None => if (listData.nonEmpty) selectIndices(0)
    case Some(si) => if (listData.contains(si)) selected = si else if (listData.nonEmpty) selectIndices(0)
  }}
  def selected:T = listData(selection.leadIndex)
  def selected_=(value:T) {selectIndices(listData.indexOf(value))}
}

class ArrowButton(left:Boolean) extends Button {
  private val width = 15
  private val thirdWidth = width / 3
  private val twoThirdsWidth = thirdWidth * 2
  private val arrowHeight = 5
  preferredSize = new Dimension(width, 10)
  minimumSize = new Dimension(width, 5)
  border = RoundedBorder()
  peer.setContentAreaFilled(false)
  focusable = false
  private val ac = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.7f)

  override protected def paintComponent(g2:Graphics2D) {
    val model = peer.getModel
    val colourToUse = if (model.isArmed && model.isPressed) {
      GUIFieldBottomColour.darker
    } else {
      GUIFieldBottomColour
    }
    val gradPaint = new GradientPaint(0, 0, GUIFieldTopColour, 0, size.height, colourToUse)
    val oldPaint = g2.getPaint
    g2.setComposite(ac)
    g2.setPaint(gradPaint)
    g2.fillRoundRect(0,0,size.width,size.height,4,4)
    g2.setPaint(oldPaint)
    g2.setColor(Color.BLACK)
    val h = size.height / 2
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    if (left) {
      g2.drawLine(thirdWidth-1,h,twoThirdsWidth-1,h-arrowHeight)
      g2.drawLine(thirdWidth-1,h,twoThirdsWidth-1,h+arrowHeight)
    } else {
      g2.drawLine(twoThirdsWidth,h,thirdWidth,h-arrowHeight)
      g2.drawLine(twoThirdsWidth,h,thirdWidth,h+arrowHeight)
    }
  }
}

class CrazyScrollPaneLayout extends ScrollPaneLayout.UIResource {
  override def layoutContainer(parent:java.awt.Container) {
    val availR = parent.getBounds()
    availR.x = 0
    availR.y = 0
    val viewPrefSize = viewport.getView.getPreferredSize
    val scrollButtonsNeeded = (viewPrefSize.width > availR.width)

    super.layoutContainer(parent)

    // This is a crazy hack as we need the vertical scroll bar to be visible but we don't want it to actually be shown.
    vsb.setBounds(-100, -100, 30, 5)

    if (scrollButtonsNeeded) {
      val leftPS = lowerLeft.getPreferredSize
      val rightPS = lowerRight.getPreferredSize
      availR.x += leftPS.width
      availR.width -= (leftPS.width + rightPS.width)

      val leftButtonBounds = new Rectangle(0,0,leftPS.width,availR.height)
      val rightButtonBounds = new Rectangle(availR.x + availR.width, 0, rightPS.width, availR.height)

      lowerLeft.setBounds(leftButtonBounds)
      viewport.setBounds(availR)
      lowerRight.setBounds(rightButtonBounds)
    } else {
      viewport.setBounds(availR)
    }
    // We do not want to vertical scroll bar to eat the mouse wheel so make it invisible here once it has taken part in the layout.
    vsb.setVisible(false)
  }
}

class CrazyScrollPane(c:Component, leftButton:ArrowButton, rightButton:ArrowButton) extends ScrollPane(c) {
  override lazy val peer: JScrollPane = new JScrollPane with SuperMixin {
    override def setLayout(layout:LayoutManager) {
      setCorner(ScrollPaneConstants.LOWER_LEFT_CORNER, leftButton.peer)
      setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, rightButton.peer)
      super.setLayout(new CrazyScrollPaneLayout())
    }
  }
}