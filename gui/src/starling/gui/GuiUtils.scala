package starling.gui

import net.miginfocom.layout.PlatformDefaults
import java.awt.image.BufferedImage
import com.jgoodies.looks.plastic.{PlasticLookAndFeel, PlasticXPLookAndFeel}
import swing.Swing._
import javax.swing.plaf.FontUIResource
import java.awt.{GraphicsEnvironment, Rectangle, Font, Color, Dimension}
import starling.gui.utils.RichColour._
import swing.{TextArea, Separator, Label}
import starling.pivot.view.swing.MigPanel
import javax.swing.{GrayFilter, UIManager}

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

  val DeletedColour = Color.LIGHT_GRAY
  val BlendedDeletedColour = TableSelectedColour.blend(DeletedColour, BlendFraction)

  val PivotTableBackgroundColour = new Color(190, 214, 248)

  val ClearColour = new Color(0,0,0,0)

  val EditedCellColour = new Color(79, 174, 232)
  val BlendedEditedCellColour = TableSelectedColour.blend(EditedCellColour, BlendFraction)
  val AddedCellColour = new Color(163, 225, 121)
  val BlendedAddedCellColour = TableSelectedColour.blend(AddedCellColour, BlendFraction)    
  val ErrorCellColour = new Color(244, 121, 124)
  val BlendedErrorCellColour = TableSelectedColour.blend(ErrorCellColour, BlendFraction)
//  val EditableCellColour = new Color(245,245,245)
  val EditableCellColour = new Color(190,203,244, 64)
  val BlendedEditableCellColour = TableSelectedColour.blend(EditableCellColour, BlendFraction)
  val BlendedHeaderColour = TableSelectedColour.blend(PanelBackgroundColour, BlendFraction)

  lazy val DisabledBorderColour = UIManager.getColor("controlShadow")
  lazy val BorderColour = UIManager.getColor("controlDkShadow")
  lazy val DisabledComboBackground = UIManager.getColor("ComboBox.disabledBackground")

  def setLookAndFeel {
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
    UIManager.put("Table.gridColor", new Color(208, 215, 229))
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
}