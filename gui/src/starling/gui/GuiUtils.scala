package starling.gui

import net.miginfocom.layout.PlatformDefaults
import java.awt.image.BufferedImage
import com.jgoodies.looks.plastic.{PlasticLookAndFeel, PlasticXPLookAndFeel}
import swing.Swing._
import javax.swing.plaf.FontUIResource
import java.awt.{GraphicsEnvironment, Rectangle, Font, Color, Dimension}
import starling.gui.utils.RichColour._
import starling.pivot.view.swing.MigPanel
import java.awt.event.{InputEvent, KeyEvent}
import swing._
import swing.event.WindowClosing
import javax.swing._

object GuiUtils {
  def LabelWithSeparator(text:String) = new MigPanel("insets 0"){
    override def enabled = super.enabled
    override def enabled_=(b:Boolean) = {
      super.enabled = b
      textLabel.enabled = b
      separator.enabled = b
      if (b) separator.foreground = colour else separator.foreground = GuiUtils.BorderColour
      separator.repaint
    }
    val colour = Color.BLUE.darker
    private val textLabel = new Label(text) {
      font = UIManager.getFont("TitledBorder.font")
      foreground = colour
    }
    private val separator = new Separator {
      foreground = colour
    }
    add(textLabel)
    add(separator, "pushx, growx")
  }

  def CentredLabelWithSeparator(text:String) = new MigPanel("insets 0") {
    opaque = false
    background = GuiUtils.ClearColour
    val colour = Color.BLUE.darker
    add(new Separator {
      foreground = colour
      background = colour.brighter
    }, "pushx, growx")
    add(new Label(text) {
      font = UIManager.getFont("TitledBorder.font")
      foreground = colour
    })
    add(new Separator {
      foreground = colour
      background = colour.brighter
    }, "pushx, growx")
  }

  def LabelTextArea(textToDisplay:String) = new TextArea {
    text = textToDisplay
    background = UIManager.getColor("panel.background")
    focusable = false
    editable = false
    lineWrap = true
    peer.setWrapStyleWord(true)
    font = UIManager.getFont("Label.font")
  }

  def createDisabledBufferedImage(image:BufferedImage):BufferedImage = {
    val greyedImage = GrayFilter.createDisabledImage(image)
    val bufferedImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_ARGB)
    bufferedImage.getGraphics.drawImage(greyedImage, 0, 0, null)
    bufferedImage
  }

  val LookAndFeel = new PlasticXPLookAndFeel

  val RightPanelSpace = (math.round(PlatformDefaults.getPanelInsets(3).getValue).toString + "px")
  val RightPanelValue = math.round(PlatformDefaults.getPanelInsets(3).getValue)
  val LeftPanelValue = math.round(PlatformDefaults.getPanelInsets(1).getValue)

  val StandardLeftIndent = "5lp:5lp:5lp"
  val ls = System.getProperty("line.separator")

  val BlockIncrement = 500
  val UnitIncrement = 10

  val TaskPageBackgroundColour = new Color(210,213,226)
  val TaskPageButtonBackgroundColour = new Color(201,205,217)
  val TaskPageButtonOverBackgroundColour = new Color(181,190,214)
  val TaskPageButtonBorderColour = new Color(166,170,182)

  val PanelBackgroundColour = new Color(246, 246, 243)

//  val StarlingBrowserBottomFadeColour = new Color(219, 232, 249)
//  val StarlingBrowserBottomFadeColour = TaskPageBackgroundColour
  val StarlingBrowserBottomFadeColour = new Color(228,228,228)
//  val StarlingBrowserTopFadeColour = new Color(244, 248, 253)
  val StarlingBrowserTopFadeColour = Color.WHITE
//  val StarlingBrowserTopFadeColour = new Color(249,249,248)
  val MouseOverColour = new Color(164, 164, 164)

  val BannerColour = new Color(198,180,145)

  val GUIFieldBottomColour = new Color(0xC8D2DE)
  val MeasureGUIFieldBottomColour = new Color(0xF6BF62)
  val GUIFieldTopColour = Color.WHITE
  val GuiFieldFilterButtonMouseOverColour = GUIFieldBottomColour.darker
  val GuiFieldBorderColour = new MigPanel().background.darker
  val GuiFieldArc = 6
  lazy val GuiFieldFont = new Label("test").font.deriveFont(11.0f)
  lazy val GuiFieldFilterNumberFont = GuiFieldFont.deriveFont(10.0f)
  val GuiFieldFilterNumberColour = Color.GREEN.darker
  val DropPanelOverColour = new Color(0,0,255,50)
  val DropPanelOverColourInvalid = new Color(255,0,0,50)
  val ExplanationPanelBackgroundColour = Color.WHITE
  val TableGridColour = new Color(208, 215, 229)

  val TableSelectedColour = new Color(195, 212, 232)

  val BlendFraction = 0.75f

  val SubtotalColour = new Color(220, 245, 224)
  val BlendedSubtotalColour = TableSelectedColour.blend(SubtotalColour, BlendFraction)
  val TotalColour = new Color(185, 236, 192)
  val BlendedTotalColour = TableSelectedColour.blend(TotalColour, BlendFraction)
//  val SubtotalTotalColour = new Color(195,249,6)
  val SubtotalTotalColour = new Color(158,202,164)
  val BlendedSubtotalTotalColour = TableSelectedColour.blend(SubtotalTotalColour, BlendFraction)
  val OtherValueTotalColour = new Color(0,246,220,30)
  val BlendedOtherValueTotalColour = TableSelectedColour.blend(OtherValueTotalColour, BlendFraction)
  val PivotTableBackgroundColour = new Color(190, 214, 248)
  val ClearColour = new Color(0,0,0,0)

  val EditableCellColour = new Color(190,203,244, 64)
  val EditedCellColour = new Color(79, 174, 232)
  val AddedBlankCellColour = new Color(163, 225, 121)
  val AddedCellColour = new Color(0xd5f7be)
  val TaintedCellColour = new Color(160, 218, 255)
  val DeletedColour = Color.LIGHT_GRAY

  /*val EditableCellColour = new Color(0xecf1f8)
  val EditedCellColour = new Color(0xFFFFD7)
  val AddedCellColour = new Color(0xd5f7be)
  val TaintedCellColour = new Color(160, 218, 255)
//  val DeletedColour = new Color(0xd3d9e2)
  val DeletedColour = new Color(0xebebeb)*/

  val ErrorCellColour = new Color(244, 121, 124)

  val RowHeaderEditableCellColour = new Color(231,235,243)

  val BlendedDeletedColour = TableSelectedColour.blend(DeletedColour, BlendFraction)
  val BlendedEditedCellColour = TableSelectedColour.blend(EditedCellColour, BlendFraction)
  val BlendedTaintedCellColour = TableSelectedColour.blend(TaintedCellColour, BlendFraction)
  val BlendedAddedCellColour = TableSelectedColour.blend(AddedCellColour, BlendFraction)
  val BlendedAddedBlankCellColour = TableSelectedColour.blend(AddedBlankCellColour, BlendFraction)
  val BlendedErrorCellColour = TableSelectedColour.blend(ErrorCellColour, BlendFraction)
  val BlendedEditableCellColour = TableSelectedColour.blend(EditableCellColour, BlendFraction)
  val BlendedHeaderColour = TableSelectedColour.blend(PanelBackgroundColour, BlendFraction)

  lazy val DisabledBorderColour = UIManager.getColor("controlShadow")
  lazy val BorderColour = UIManager.getColor("controlDkShadow")
  lazy val DisabledComboBackground = UIManager.getColor("ComboBox.disabledBackground")

  def setLookAndFeel() {
    val platform = PlatformDefaults.getPlatform
    if (platform == PlatformDefaults.MAC_OSX) {
      PlatformDefaults.setPlatform(PlatformDefaults.WINDOWS_XP)
    }

//    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    PlasticLookAndFeel.setTabStyle(PlasticLookAndFeel.TAB_STYLE_METAL_VALUE)
    UIManager.setLookAndFeel(LookAndFeel)
    val backColour = PanelBackgroundColour

    UIManager.put("Panel.background", backColour)
    //        UIManager.put("Panel.background", new Color(80, 151, 237))
    UIManager.put("Button.background", backColour)
    UIManager.put("ComboBox.background", backColour)
    UIManager.put("RadioButton.background", backColour)
    UIManager.put("CheckBox.background", backColour)
    UIManager.put("SplitPane.background", backColour)
    UIManager.put("MenuItem.background", backColour) // JGoodies looks uses this for the combo box popup.
    UIManager.put("control", backColour) // This is used for the scroll bar buttons.
    UIManager.put("Viewport.background", backColour)
    UIManager.put("Table.gridColor", TableGridColour)
    val blueFocusBorderColor = new Color(166, 202, 240)
    UIManager.put("Table.focusSelectedCellHighlightBorder", LineBorder(blueFocusBorderColor))
    UIManager.put("List.focusSelectedCellHighlightBorder", LineBorder(blueFocusBorderColor))
    UIManager.put("Table.selectionBackground", TableSelectedColour)
    UIManager.put("Table.selectionForeground", Color.BLACK)
    UIManager.put("List.selectionBackground", TableSelectedColour)
    UIManager.put("List.selectionForeground", Color.BLACK)
    UIManager.put("ComboBox.selectionBackground", TableSelectedColour)
    UIManager.put("ComboBox.selectionForeground", Color.BLACK)
    UIManager.put("TableHeader.background", backColour)
    UIManager.put("ScrollPane.background", backColour)
    UIManager.put("TabbedPane.selected", GuiUtils.StarlingBrowserTopFadeColour) // This is the colour of the tabs when selected.
    //        UIManager.put("TabbedPane.selected", Color.RED)
    val buttonSelectColour = new Color(208, 208, 208)
    UIManager.put("ToggleButton.select", buttonSelectColour);
    UIManager.put("Button.select", buttonSelectColour)
    UIManager.put("MenuItem.selectionBackground", MouseOverColour)
    UIManager.put("SplitPaneDivider.draggingColor", MouseOverColour)

    UIManager.put("JXMonthView.font", new FontUIResource("Dialog", Font.PLAIN, 10))

    // I want to disable ctrl page down and page up in ListViews, Tables and ScrollPanes so that you can change tabs when these have focus.
    val lv = new ListView(List("Bla")) {
      peer.getInputMap(JComponent.WHEN_FOCUSED).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_DOWN_MASK))
      peer.getInputMap(JComponent.WHEN_FOCUSED).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_DOWN_MASK))
    }
    new ScrollPane(lv) {
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_DOWN_MASK))
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_DOWN_MASK))
    }
    new Table() {
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_DOWN_MASK))
      peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent.remove(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_DOWN_MASK))
    }
  }

  def onScreen(rect:Rectangle) = {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val screens = ge.getScreenDevices
    val minX = screens.map(screen => {
      val bounds = screen.getDefaultConfiguration.getBounds
      bounds.x
    }).foldLeft(0)((x1,x2) => {math.min(x1,x2)})
    val maxX = screens.map(screen => {
      val bounds = screen.getDefaultConfiguration.getBounds
      bounds.x + bounds.width
    }).foldLeft(0)((x1,x2) => {math.max(x1,x2)})
    val minY = screens.map(screen => {
      val bounds = screen.getDefaultConfiguration.getBounds
      bounds.y
    }).foldLeft(0)((y1,y2) => {math.min(y1,y2)})
    val maxY = screens.map(screen => {
      val bounds = screen.getDefaultConfiguration.getBounds
      bounds.y + bounds.height
    }).foldLeft(0)((y1,y2) => {math.max(y1,y2)})

    val screenArea = new Rectangle(minX,minY,maxX-minX,maxY-minY)
    // Work out if the top 200 by 200 pixels of the supplied rect is in the screen. When maximized, the position is -4 so add 4 to make it fit inside the screen.
    screenArea.contains(new Rectangle(rect.x + 4, rect.y + 4, 200, 200))
  }

  def showInFrame(comp:Component, pack0:Boolean=true) {
    new Frame {
      title = "Test frame"
      reactions += {case WindowClosing(_) => System.exit(0)}
      contents = comp
      if (pack0) {
        pack()
      } else {
        bounds = new Rectangle(100, 100, 500, 400)
      }
      centerOnScreen()
      visible = true
    }
  }

  def resizeTableColumnsToFit(table:JTable, font0:Font) {
    val l = new Label("sdkfjh") {font = font0}
    for (col <- (0 until table.getColumnCount)) {
      var width = 0
      for (row <- (0 until table.getRowCount)) {
        l.text = table.getValueAt(row, col).toString
        width = math.max(width, l.preferredSize.width)
      }
      width += 5
      table.getColumnModel.getColumn(col).setPreferredWidth(width)
      table.getColumnModel.getColumn(col).setMinWidth(width)
    }
  }
}