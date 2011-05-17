package starling.pivot.view.swing

import java.awt.event.{ItemListener, ItemEvent}
import javax.swing.JComboBox

/**
 * A helper trait that allows scala swing components that consist of lists of items
 * (so far only combo boxes are supported)
 * to be notified when their selected item changes. In an ideal world we would not need to write such things
 * but the scala swing reactions framework does not seem to respond to such events. Maybe this is something
 * that will be fixed in scala 2.8.
 * So far the items are assumed always to be strings
 */
trait ItemListening {
  /** Will be called when the selectio have changed*/
  def selectionChanged(value : String)
  val peer : javax.swing.JComponent

  peer.asInstanceOf[JComboBox].addItemListener(new ItemListener {
      def itemStateChanged(e: java.awt.event.ItemEvent) {
        if(e.getStateChange == ItemEvent.SELECTED) {
          selectionChanged(e.getItem.toString)
        }
      }
  })
}