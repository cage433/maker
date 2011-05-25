package starling.pivot.view.swing

import scala.swing.Component
import org.jdesktop.jxlayer.JXLayer
import javax.swing.JComponent
import org.jdesktop.jxlayer.plaf.LayerUI

class SXLayer[T <: java.awt.Component](wrappedComponent: T, layerUI: LayerUI[T]) extends Component {
  override lazy val peer: JXLayer[T] = new JXLayer[T](wrappedComponent, layerUI)
  def getView = peer.getView
  def setLayerEventMask(mask: Long) {
    peer.setLayerEventMask(mask)
  }
}

class SXLayerScala[T <: Component](wrappedComponent: T, layerUI: LayerUI[JComponent]) extends Component {
  override lazy val peer: JXLayer[JComponent] = new JXLayer[JComponent](wrappedComponent.peer, layerUI)
  def getView = peer.getView
  def getScalaComponent = wrappedComponent
  def setLayerUI(layerUI: LayerUI[JComponent]) {
    peer.setUI(layerUI)
  }
}