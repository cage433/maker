package starling.gui.custom

import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import net.miginfocom.swing.MigLayout
import javax.swing.{JPanel, JComponent}
import org.jdesktop.jxlayer.JXLayer
import java.awt.{BorderLayout, FlowLayout}
import swing.Component

class TopRightLayerUI(component:Component) extends AbstractLayerUI[JComponent] {
  private val contentPanel = new JPanel(new MigLayout("insets 1 0 0 1", "push[p]", "[p]push")) {
    setOpaque(false)
    add(component.peer, "push,grow")
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