package starling.pivot.view.swing.fieldchoosers

import starling.pivot.model.PivotTableModel
import starling.pivot.view.swing._
import starling.gui.StarlingIcons
import starling.gui.GuiUtils._
import starling.pivot.{Field, OtherLayoutInfo}
import starling.pivot.FieldChooserType._
import swing.event.{Event, MouseClicked}
import java.awt.{Dimension, Rectangle}

class FieldGroupSeparator(group:String, collapsed:Boolean, onCollapseOrExpand:(String,Boolean)=>Unit) extends MigPanel("insets 2lp 2lp 3lp 0", "[p]0[p]") {
  private val expandButton = new ImageButton(StarlingIcons.im("/icons/scroll_down.png"), {onCollapseOrExpand(group, true)})
  private val collapseButton = new ImageButton(StarlingIcons.im("/icons/scroll_up.png"), {onCollapseOrExpand(group, false)})
  private val labelWithSeparator = LabelWithSeparator(group)
  reactions += {case MouseClicked(`labelWithSeparator`,_,_,2,_) => {onCollapseOrExpand(group, collapsed)}}
  listenTo(labelWithSeparator.mouse.clicks)
  labelWithSeparator.enabled = !collapsed
  add(labelWithSeparator, "push, grow")
  val buttonToAdd = if (collapsed) expandButton else collapseButton
  add(buttonToAdd)
}

class FieldListComponent(model:PivotTableModel, otherLayoutInfo:OtherLayoutInfo,
                         viewUI:PivotTableViewUI, mainComponent:PivotTableView)
        extends MigPanel("insets 1, wrap 1, gap 0px", "fill, grow") with DropTarget {
  opaque = false
  
  private val fields = model.getFields(FieldList)

  private val guiFieldsMap = Map() ++ fields.fields.map(field => {
    val shouldShowDepthPanel = false
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = false
    val treeDetails = model.treeDetails
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = false
    val realMeasureField = model.isMeasureField(field)
    val subTotalToggleVisible = false
    val props = GuiFieldComponentProps(field, FieldList, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField, treeDetails,
      (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => (), filterData, transformData, otherLayoutInfo,
      (_field, from) => (), subTotalToggleVisible, viewUI, mainComponent)
    (field -> new GuiFieldComponent(props))
  })

  private val groupedFields = model.getGroupedFields(FieldList)
  case class GUIFieldGroup(name:String, guiFields:List[GuiFieldComponent])
  private val guiFieldGroups = groupedFields.map(groupedField => {
    GUIFieldGroup(groupedField.name, groupedField.fieldList.fields.map(guiFieldsMap(_)))
  })

  private var collapsedGroups = Set[String]()

  private def updateLayout() {
    removeAll
    for (guiFieldGroup <- guiFieldGroups) {
      add(new FieldGroupSeparator(guiFieldGroup.name, collapsedGroups.contains(guiFieldGroup.name), (_name,cOrE) => groupCollapsedOrExpanded(_name,cOrE)))
      for (field <- guiFieldGroup.guiFields) {
        if (field.props.field.name.toLowerCase.trim.contains(filterText.trim.toLowerCase) && !collapsedGroups.contains(guiFieldGroup.name)) {
          add(field, "gapbefore 5lp")
        }
      }
      add(SpacerPanel("insets 0"))
    }
    revalidate()
    repaint()
  }

  private var filterText = ""

  def setTextFilter(text:String) {
    filterText = text
    updateLayout()
  }

  private def groupCollapsedOrExpanded(groupName:String, expanded:Boolean) {
    if (expanded) {
      collapsedGroups -= groupName
    } else {
      collapsedGroups += groupName
    }
    updateLayout()
  }

  updateLayout()

  def fieldChooserType = FieldList

  def dropBounds(draggedField:Field) = {
    val screenPoint = peer.getLocationOnScreen
    new Rectangle(screenPoint.x, screenPoint.y, size.width, size.height) :: Nil
  }

  def show(draggedField:Field) {}
  def hide() {}

  def reset() {
    guiFieldsMap.values.foreach(_.namePanel.reset())
  }
}

case class SpacerPanel(constraints:String) extends MigPanel(constraints) {
  preferredSize = new Dimension(1, 5)
}