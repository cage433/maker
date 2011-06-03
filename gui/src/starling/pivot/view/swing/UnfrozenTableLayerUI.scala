package starling.pivot.view.swing

import fieldchoosers.RowComponent
import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import starling.gui.GuiUtils
import javax.swing.{JPanel, JComponent}
import net.miginfocom.swing.MigLayout
import org.jdesktop.jxlayer.JXLayer
import java.awt.{BorderLayout, FlowLayout}

class UnfrozenTableLayerUI(rowComponent:RowComponent) extends AbstractLayerUI[JComponent] {
  private val contentPanel = new JPanel(new MigLayout("insets 0")) {
    setBackground(GuiUtils.ClearColour)
    add(rowComponent.peer)
  }

  override def installUI(c: JComponent) = {
    super.installUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.setLayout(new BorderLayout)
    glassPane.add(contentPanel)
  }

  override def uninstallUI(c: JComponent) = {
    super.uninstallUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.remove(contentPanel)
    glassPane.setLayout(new FlowLayout)
  }
}