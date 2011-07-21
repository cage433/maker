package starling.pivot.view.swing

import java.awt.event.MouseEvent
import javax.swing._
import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import org.jdesktop.jxlayer.JXLayer
import net.miginfocom.swing.MigLayout
import starling.pivot.FieldChooserType._
import starling.gui.GuiUtils._
import collection.mutable.{ListBuffer}
import java.awt.{AWTEvent, Dimension}
import starling.pivot.ColumnStructure
import starling.pivot.Position._
import ColumnStructure._
import starling.pivot.model.PivotTableModel
import GuiFieldComponent._
import starling.gui.{GuiUtils, StarlingIcons}
import starling.pivot.{OtherLayoutInfo, Field}
import swing.event.{Event, MouseClicked}

object FieldChooser {
  def apply(layoutManager:MigLayout, dragInfo:DragInfo, defaultText:String, showFilter:Boolean,
            fieldChooserType:FieldChooserType, model:PivotTableModel, layerUI:PivotTableLayerUI, parent:PivotTableView,
            otherLayoutInfo:OtherLayoutInfo): SXLayer[FieldChooser] = {
    val panel = new FieldChooser(layoutManager, dragInfo, defaultText, showFilter, fieldChooserType, model, layerUI, parent, otherLayoutInfo)
    val pivotPanelLayerUI:FieldChooserLayerUI = new FieldChooserLayerUI(panel)
    return new SXLayer[FieldChooser](panel, pivotPanelLayerUI) {
      setLayerEventMask(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK
              //| AWTEvent.MOUSE_WHEEL_EVENT_MASK - mouse wheel event's will pass through
              | AWTEvent.KEY_EVENT_MASK | AWTEvent.FOCUS_EVENT_MASK)
    }
  }
}

object ColumnDragAction extends Enumeration {
  type ColumnDragAction = Value
  val Valid, InValid, RemoveReplace = Value
}

import ColumnDragAction._

trait DragInfo {
  def getSource: FieldChooser
  def getDraggedField: GuiFieldComponent
  def getDestination: FieldChooser
  def isBeingDragged: Boolean
  def setDestination(destinationPanel: FieldChooser)
  def setPosition(position: Int)
  def getPosition: Int
  def reset
  def setColumnData(data:ColumnStructure)
  def getColumnData:ColumnStructure
  def setColumnDragAction(action:ColumnDragAction)
  def getColumnDragAction:ColumnDragAction
}

private class FieldChooserLayerUI(panel: FieldChooser) extends AbstractLayerUI[FieldChooser] {
  protected override def processMouseEvent(e: MouseEvent, l: JXLayer[_ <: FieldChooser]):Unit = {
    var id: Int = e.getID
    if (id == MouseEvent.MOUSE_ENTERED) {
      panel.mouseEntered
    }
    else if (id == MouseEvent.MOUSE_EXITED && l.getMousePosition() == null) {
      panel.mouseExited
    }
    else if (id == MouseEvent.MOUSE_RELEASED) {
      panel.mouseReleased
    }
  }

  override def processMouseMotionEvent(e: MouseEvent, l: JXLayer[_ <: FieldChooser]) = panel.mouseOver
}

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

case class SpacerPanel(constraints:String) extends MigPanel(constraints) {
  preferredSize = new Dimension(1, 5)
}

case class SubTotalToggled(field:Field, location:FieldChooserType) extends Event

class FieldChooser(layoutManager: MigLayout, dragInfo: DragInfo, val defaultText: String,
                   val showFilter:Boolean, val fieldChooserType: FieldChooserType, model:PivotTableModel,
                   layerUI:PivotTableLayerUI, parent:PivotTableView, otherLayoutInfo:OtherLayoutInfo) extends JPanel(layoutManager) {
  setOpaque(false)
  private val horizontal = fieldChooserType match {
    case FieldList => false
    case _ => true
  }
  val col = fieldChooserType match {
    case Columns => true
    case _ => false
  }
  private val fieldList = model.getFields(fieldChooserType)
  private val currentColumnDataFields = model.columns.dataFields.toSet
  private val guiFieldsMap = Map() ++ fieldList.fields.map(field => {
    val shouldShowDepthPanel = ((fieldChooserType == Rows || fieldChooserType == Columns) && model.treeDetails.maxTreeDepths.getOrElse(field, 0) > 1)
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = (fieldChooserType == Rows || fieldChooserType == Columns)
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = currentColumnDataFields.contains(field)
    val realMeasureField = model.isDataField(field)
    val subTotalToggleVisible = {
      (fieldChooserType == Rows) && otherLayoutInfo.totals.rowSubTotals && (fieldList.fields.last != field)
    }
    val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField,
      model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => switchMeasureField(_field, from), layerUI, this, filterData, transformData, otherLayoutInfo,
      (_field, from) => subTotalSubTotalToggle(_field, from), subTotalToggleVisible)
    (field -> new GuiFieldComponent(props))
  })
  private val guiFields = fieldList.fields.map(field => guiFieldsMap(field))
  private val groupedFields = model.getGroupedFields(fieldChooserType)
  case class GUIFieldGroup(name:String, guiFields:List[GuiFieldComponent])
  private val guiFieldGroups = groupedFields.map(groupedField => GUIFieldGroup(groupedField.name, groupedField.fieldList.fields.map(guiFieldsMap(_))))
  case class ComponentHolder(component:JComponent, constraints:String)
  private val componentHolders = new ListBuffer[ComponentHolder]

  private var collapsedGroups = Set[String]()

  private val columnDataFieldsSet = model.columns.dataFields.toSet
  private def switchMeasureField(field:Field, from:FieldChooserType) {
    val newCS = model.columns.flipIsData(field)
    publishFieldStateChange(field, newCS, from)
  }
  private def subTotalSubTotalToggle(field:Field, from:FieldChooserType) {
    parent.publish(SubTotalToggled(field, from))
  }
  private val columnField:Option[GUITreeField] = {
    if (col) {
      val colGuiFields = Map() ++ (RootField :: model.columns.allFields).map(field => {
        val shouldShowDepthPanel = model.treeDetails.maxTreeDepths.getOrElse(field, 0) > 1
        val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
        val showOther = (fieldChooserType == Rows || fieldChooserType == Columns)
        val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
        val currentlyActingAsMeasure = currentColumnDataFields.contains(field)
        val realMeasureField = model.isDataField(field)
        val subTotalToggleVisible = {
          otherLayoutInfo.totals.columnSubTotals && !model.columns.isBottomField(field)
        }
        val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
          currentlyActingAsMeasure, realMeasureField,
          model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
          (_field, from) => switchMeasureField(_field, from), layerUI, this, filterData, transformData, otherLayoutInfo,
          (_field, from) => subTotalSubTotalToggle(_field, from), subTotalToggleVisible)
        (field -> new GuiFieldComponent(props))
      })
      Some(GUITreeField(model.columns, colGuiFields, showFilter, defaultText, parent))
    } else {
      None
    }
  }
  private val columnGuiFields:List[GuiFieldComponent] = {
    if (col) {
      columnField.get.guiFieldsMap.values.toList
    } else {
      List()
    }
  }

  def groupCollapsedOrExpanded(groupName:String, expanded:Boolean) {
    if (expanded) {
      collapsedGroups -= groupName
    } else {
      collapsedGroups += groupName
    }
    populateComponents
    updateLayout()
  }
    
  private def populateComponents {
    componentHolders.clear
    // If we are using a vertical field chooser (i.e. the main field chooser on the left), we want to use groups.
    if (!horizontal) {
      for (guiFieldGroup <- guiFieldGroups) {
        componentHolders += ComponentHolder(new FieldGroupSeparator(guiFieldGroup.name, collapsedGroups.contains(guiFieldGroup.name), (_name,cOrE) => groupCollapsedOrExpanded(_name,cOrE)).peer, "")
        for (field <- guiFieldGroup.guiFields) {
          if (field.props.field.name.toLowerCase.trim.contains(filterText.trim.toLowerCase) && !collapsedGroups.contains(guiFieldGroup.name)) componentHolders += ComponentHolder(field.peer, "gapbefore 5lp")
        }
        componentHolders += ComponentHolder(SpacerPanel("insets 0").peer, "")
      }
    } else {
      if (!col) {
        for (field <- guiFields) {
          componentHolders += ComponentHolder(field.peer, "")
        }
      } else {
        // It is the column field chooser so do something special.
        for (column <- columnField) {
          componentHolders += ComponentHolder(column.mainComponent, "")
        }
      }
    }
  }

  private var filterText = ""

  def setTextFilter(text:String) {
    filterText = text
    populateComponents
    updateLayout()
  }

  populateComponents

  def scrolling {
    // In a scroll pane scrolling so remove the raised image and reset the view.
    resetImage
    // TODO - put this back in but make sure it works on Dave's machine.
    // Now ensure that the field we are currently under is selected properly.
    /*val pointerInfo = MouseInfo.getPointerInfo
    if (pointerInfo != null) {
      val mousePoint = pointerInfo.getLocation
      var keepGoing = true
      for (guiField <- guiFields if (keepGoing)) {
        val componentPoint = new Point(mousePoint)
        SwingUtilities.convertPointFromScreen(componentPoint, guiField.mainGUIComponent)
        if (guiField.mainGUIComponent.contains(componentPoint)) {
          guiField.mouseInComponent(guiField.mainGUIComponent)
          keepGoing = false
        }
      }
    }*/
  }

  def resetImage {
    layerUI.resetOverImage
    layerUI.getOverImageSource.map(_.resetDisplayState)
  }

  val baseLayout = layoutManager.getLayoutConstraints

  // Do general set up for each kind of field chooser.
  fieldChooserType match {
    case Filter => setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, GuiUtils.BorderColour))
    case Rows => if (!fieldList.isEmpty) setAlignment("al right")
    case _ =>
  }

  private def setAlignment(alignment:String) {
    layoutManager.setLayoutConstraints(baseLayout + ", " + alignment)
  }

  def mouseEntered {
    if (!dragInfo.isBeingDragged) {
      return
    }
    dragInfo.setDestination(this)
  }

  def mouseExited {
    if (dragInfo.isBeingDragged) {
      dragInfo.setDestination(null)
      dragInfo.setColumnData(null)
      dragInfo.setColumnDragAction(ColumnDragAction.InValid)
      resetPanelState
    }
  }
  private var lastIndexToInsert = -1
  private var lastTempComponent:ComponentHolder = null
  private var lastTempComponentName = ""

  def mouseOver {
    val destination = dragInfo.getDestination
    if (dragInfo.isBeingDragged && (destination != null)) {
      if (destination != this) {
        destination.mouseOver
      } else {
        if (!col) {
          val position = getMousePosition()
          // Find out if it is over any of the GUIFields.
          if (position != null) {
            val draggedField = dragInfo.getDraggedField
            val guiIndex = if (guiFields.filter(_ == draggedField).isEmpty) {
              // This field chooser doesn't contain the dragged field.
              val guiFieldContainingMouse = guiFields.filter(_.bounds.contains(position))
              if (guiFieldContainingMouse.isEmpty) {
                // On the chooser but not over a gui field so decide which end to place it.
                if (horizontal) {
                  // A standard field chooser.
                  if (guiFields.isEmpty) {
                    // There is nothing in this field chooser so place dragged component in the 1st position.
                    0
                  } else {
                    // We are either right of all components or left of all components, or over the tempComponent already drawn.
                    if ((lastTempComponent != null) && lastTempComponent.component.getBounds().contains(position)) {
                      -1
                    } else {
                      if (position.x < guiFields(0).bounds.x) {
                        0
                      } else {
                        guiFields.size
                      }
                    }
                  }
                } else {
                  // The vertical field chooser on the left.
                  // Don't draw anything for now.
                  -3
                }
              } else {
                // Over a gui field so decide which side to place it.
                val guiField = guiFieldContainingMouse(0)
                val guiComponent = guiField
                if (horizontal) {
                  // A standard field chooser.
                  if (position.x > (guiComponent.bounds.x + guiComponent.size.width / 2)) (positionOfField(guiField) + 1) else positionOfField(guiField)
                } else {
                  // The vertical field chooser on the left.
                  // Don't draw anything for now.
                  -3
                }
              }
            } else {
              // This field chooser already contains this field.
              if (horizontal) {
                if ((position.x >= draggedField.bounds.x) && (position.x < draggedField.bounds.x + draggedField.bounds.width)) {
                  // I'm in bounds of the component being dragged.
                  -2
                } else if ((lastTempComponent != null) && lastTempComponent.component.getBounds().contains(position)) {
                  // I'm in the bounds of the component that is already being drawn so nothing should be drawn.
                  -1
                } else {
                  val indexOfRightField = positionOfField(draggedField) + 1
                  val indexOfLeftField = positionOfField(draggedField) - 1
                  if ((indexOfRightField < guiFields.size) &&
                          (guiFields(indexOfRightField).bounds.contains(position)) &&
                          (position.x <= (guiFields(indexOfRightField).bounds.x + guiFields(indexOfRightField).size.width / 2))) {
                    // In the left section of the component to the right of the dragged component.
                    -2
                  } else if ((indexOfLeftField >= 0) &&
                          (guiFields(indexOfLeftField).bounds.contains(position)) &&
                          (position.x >= (guiFields(indexOfLeftField).bounds.x + guiFields(indexOfLeftField).size.width / 2))) {
                    // In the right section of the component to the left of the dragged component.
                    -2
                  } else {
                    // We are basically back to normal now that we have dealt with the special cases when the field is already contained in the field chooser.
                    val guiFieldContainingMouse = guiFields.filter(_.bounds.contains(position))
                    if (guiFieldContainingMouse.isEmpty) {
                      // On the chooser but not over a gui field so decide which end to place it.
                      // We are either right of all components or left of all components, or over the tempComponent already drawn.
                      if (position.x < guiFields(0).bounds.x) {
                        if (guiFields(0) == draggedField) {
                          -1
                        } else {
                          0
                        }
                      } else {
                        if (guiFields(guiFields.size - 1) == draggedField) {
                          -1
                        } else {
                          guiFields.size
                        }
                      }
                    } else {
                      // Over a gui field so decide which side to place it.
                      val guiField = guiFieldContainingMouse(0)
                      val guiComponent = guiField
                      if (position.x > (guiComponent.bounds.x + guiComponent.size.width / 2)){
                        (positionOfField(guiField) + 1)}
                      else {
                        positionOfField(guiField)}
                    }
                  }
                }
              } else {
                // This is the vertical field chooser. Will never want to draw this as it is already there being drawn.
                -3
              }
            }

            if ((lastIndexToInsert != guiIndex) && (guiIndex != -1) && (guiIndex != -2) && (guiIndex != -3)) {
              componentHolders -= lastTempComponent
              lastTempComponent = ComponentHolder(TempGuiFieldNamePanel(draggedField.props.field.name).peer, "")
              componentHolders.insert(guiIndex, lastTempComponent)
              lastIndexToInsert = guiIndex
              dragInfo.setPosition(guiIndex)
              updateLayout(fieldChooserType == Rows)
            } else if (guiIndex == -2) {
              // Shouldn't draw anything here.
              componentHolders -= lastTempComponent
              lastIndexToInsert = guiIndex
              lastTempComponent = null
              dragInfo.setPosition(-1)
              updateLayout()
            } else if (guiIndex == -3) {
              // Field chooser on the left
              dragInfo.setPosition(0)
              lastIndexToInsert = guiIndex
            }
          }
        } else {
          mouseOverCol
        }
      }
    }
  }

  private var lastColPosToInsert:(Option[GuiFieldComponent], Position, ColumnDragAction) = (None, Other, InValid)
  private var tempColComponent:TempGuiFieldNamePanel = null

  private def mouseOverCol = {
    val mousePosition = getMousePosition()
    // Find out if it is over any of the GUIFields.
    if (mousePosition != null) {      
      def getPosDumbBell(gf:GuiFieldComponent) = {
        val gfc = gf
        val bounds = gfc.bounds
        val sx = bounds.x
        val sy = bounds.y
        val ex = sx + gfc.size.width
        val ey = sy + gfc.size.height
        val midy = sy + gfc.size.height / 2.0
        
        val mx = mousePosition.x
        val my = mousePosition.y

        if (mx < (sx + HorizontalGap)) {
          // Left section.
          if (my < (sy + VerticalGap)) {
            Top
          } else if (my > (ey - VerticalGap)) {
            Bottom
          } else {
            Left
          }
        } else if ((mx > (sx + HorizontalGap)) && (mx < (ex - HorizontalGap))) {
          // Middle section.
          if (my < midy) {
            Top
          } else {
            Bottom
          }
        } else {
          // Right section.
          if (my < (sy + VerticalGap)) {
            Top
          } else if (my > (ey - VerticalGap)) {
            Bottom
          } else {
            Right
          }
        }
      }

      val draggedField = dragInfo.getDraggedField
      val fieldPosition = if (columnGuiFields.filter(_ == draggedField).isEmpty) {
        // This field chooser doesn't contain the dragged field.
        val guiFieldContainingMouse = columnGuiFields.find(_.bounds.contains(mousePosition))
        guiFieldContainingMouse match {
          case None => {
            // On the chooser but not over a gui field so decide where to place it.

            // It can be either to the right or left of any column panel, or over the tempComponent.

            if ((tempColComponent != null) && tempColComponent.bounds.contains(mousePosition)) {
              (None, Other, Valid)
            } else {
              // We are to the right of the area so add as a top level child.
              val children = columnField.get.column.children
              if (children.nonEmpty) {
                val rightColStructure = children.reverse.head
                val fieldToUse = rightColStructure.field
                val guiField = columnField.get.guiFieldsMap(fieldToUse)
                (Some(guiField), Right, Valid)
              } else {
                (Some(columnField.get.guiFieldsMap(RootField)), Bottom, Valid)
              }
            }
          }
          case Some(gf) => {
            // We are over the field but are we top, left bottom or right?
            val pos = getPosDumbBell(gf)
            (Some(gf), pos, Valid)
          }
        }
      } else {
        // This field chooser already contains this field.

        if (draggedField.bounds.contains(mousePosition)) {
          // Inside the field that is being dragged so don't need to do anything.
          (None, Other, InValid)
        } else {
          val guiFieldContainingMouse = columnGuiFields.find(_.bounds.contains(mousePosition))
          guiFieldContainingMouse match {
            case None => {
              // We are not over a gui field so we are to the right of all gui fields, or over the temp field.
              if ((tempColComponent != null) && tempColComponent.bounds.contains(mousePosition)) {
                (None, Other, Valid)
              } else {
                val children = columnField.get.column.children
                if (children.nonEmpty) {
                  val rightColStructure = children.reverse.head
                  val fieldToUse = rightColStructure.field
                  // We don't want the temp gui field draw if it is just going to be in the same place as the original field.
                  if (rightColStructure.children.nonEmpty || (fieldToUse != draggedField.props.field)) {
                    val guiField = columnField.get.guiFieldsMap(fieldToUse)
                    (Some(guiField), Right, RemoveReplace)
                  } else {
                    (None, Other, InValid)
                  }
                } else {
                  (Some(columnField.get.guiFieldsMap(RootField)), Bottom, RemoveReplace)
                }
              }
            }
            case Some(gf) => {
              // We are over a gui field
              val guiFieldContainingMouse = columnGuiFields.find(_.bounds.contains(mousePosition))
              guiFieldContainingMouse match {
                case None => throw new Exception("We should be over a gui field here")
                case Some(gf) => {
                  // We don't want the temp gui field drawn if it is just going to be in the same place as the original field.
                  val pos = getPosDumbBell(gf)
                  val currentStructure = columnField.get.column
                  val isData = columnDataFieldsSet.contains(draggedField.props.field)
                  val newStructure = currentStructure.remove(draggedField.props.field).add(draggedField.props.field, isData, gf.props.field, pos)
                  if (currentStructure != newStructure) {
                    (Some(gf), pos, RemoveReplace)
                  } else {
                    // There would be no effect by dropping the field here so don't allow it.
                    (None, Other, InValid)
                  }
                }
              }
            }
          }
        }
      }

      val (maybeGUIField, position, isValid) = fieldPosition

      if (lastColPosToInsert != fieldPosition && maybeGUIField.isDefined && ((isValid == Valid) || (isValid == RemoveReplace))) {
        lastColPosToInsert = fieldPosition
        maybeGUIField match {
          case None => throw new Exception("We should always have a gui field here")
          case Some(gf) => {
            columnField match {
              case None => throw new Exception("We should always be able to find the column field here")
              case Some(colField) => {
                val currentColumnStructure = colField.column
                val pos = position
                val isData = if (colField.column.allFields.contains(draggedField.props.field)) {
                  // Already in the column structure so keep whatever the current isData state.
                  columnDataFieldsSet.contains(draggedField.props.field)
                } else {
                  model.isDataField(draggedField.props.field)
                }
                val newColumnStructure = currentColumnStructure.add(TempField, isData, gf.props.field, pos)
                tempColComponent = TempGuiFieldNamePanel(draggedField.props.field.name)
                colField.resetImageState
                colField.populateComponents(newColumnStructure)
                colField.updateLayout(Some((tempColComponent, Some(gf), pos)))

                dragInfo.setColumnDragAction(isValid)
                dragInfo.setColumnData(newColumnStructure)
              }
            }
          }
        }
      } else if (lastColPosToInsert != fieldPosition && (isValid == InValid)) {
        lastColPosToInsert = fieldPosition
        // The fieldPosition has changed but gui is not defined.
        columnField match {
          case None => throw new Exception("We should always be able to find the column field here")
          case Some(colField) => {
            val newColumnStructure = colField.column.remove(TempField)
            colField.resetImageState
            colField.populateComponents(newColumnStructure)
            colField.updateLayout()
          }
        }
        dragInfo.setColumnDragAction(isValid)
      }
    }
  }

  def addGuiComponentToEndOfChooser(name:String) {
    if (col) {
      val colField = columnField.get
      val cs = colField.column
      val newColumnStructure = cs.addDataField(TempField)
      tempColComponent = TempGuiFieldNamePanel(name)
      colField.resetImageState
      colField.populateComponents(newColumnStructure)
      colField.updateLayout(Some((tempColComponent, None, Other)))
    } else if (fieldChooserType != FieldList) {
      componentHolders += ComponentHolder(TempGuiFieldNamePanel(name).peer, "")
      updateLayout(fieldChooserType == Rows)
    }
  }

  def resetPanelState {
    resetPanelStateWithoutUpdate
    columnField.foreach(_.updateLayout())
    updateLayout()
  }

  def resetPanelStateWithoutUpdate {
    lastIndexToInsert = -1
    lastTempComponent = null
    lastTempComponentName = ""

    tempColComponent = null
    lastColPosToInsert = (None, Other, InValid)

    columnField.foreach(_.resetImageState)
    columnField.foreach(_.resetComponents)

    populateComponents
  }

  def mouseReleased: Unit = {
    if (dragInfo.isBeingDragged) {
      val draggedField = dragInfo.getDraggedField
      val destinationPanel = dragInfo.getDestination
      if (destinationPanel != null) {
        destinationPanel.resetPanelStateWithoutUpdate
        if (!destinationPanel.col) {
          // Want to put into destination panel if the position has changed.
          var position = dragInfo.getPosition
          if (position != -1) {
            val destContains = destinationPanel.contains(draggedField.props.field)

            if (destContains) {
              if (destinationPanel.positionOfField(draggedField) != position) {                
                position = if (destContains && (destinationPanel.positionOfField(draggedField) < position)) position - 1 else position
                destinationPanel.publishFieldStateChange(draggedField.props.field, position, fieldChooserType)
              }
            } else {
                position = if (destContains && (destinationPanel.positionOfField(draggedField) < position)) position - 1 else position
                destinationPanel.publishFieldStateChange(draggedField.props.field, position, fieldChooserType)              
            }
          }
        } else {
          // The destination is the column and data area.
          if (dragInfo.getColumnDragAction == Valid) {
            val newColumnStructure = dragInfo.getColumnData.replace(TempField, draggedField.props.field)
            destinationPanel.publishFieldStateChange(draggedField.props.field, newColumnStructure, fieldChooserType)
          } else if (dragInfo.getColumnDragAction == RemoveReplace) {
            val newColumnStructure = dragInfo.getColumnData.remove(draggedField.props.field).replace(TempField, draggedField.props.field)
            destinationPanel.publishFieldStateChange(draggedField.props.field, newColumnStructure, fieldChooserType)
          } else {
            draggedField.namePanel.reset
          }
        }
      } else {
        draggedField.namePanel.reset
      }
    }    
  }

  def fieldForName(name:String) = {
    fieldList.fields.find(_.name == name) match {
      case Some(field)=>field
      case None => throw new Exception("No field found with name " + name)
    }
  }

  def autoMoveField(field:Field) {
    fieldChooserType match {
      case FieldList => {
        if (model.isDataField(field)) {
          model.publishFieldStateChange(field, model.getFields(Columns).size, fieldChooserType, Columns)
        } else {
          model.publishFieldStateChange(field, model.getFields(Rows).size, fieldChooserType, Rows)
        }
      }
      case _ => model.publishFieldStateChange(field, 0, fieldChooserType, FieldList)
    }

  }

  def fieldChooserThatThisFieldWillAutoMoveTo(field:Field) = {
    fieldChooserType match {
      case FieldList => {
        if (model.isDataField(field)) {
          parent.getFieldChooser(Columns)
        } else {
          parent.getFieldChooser(Rows)
        }
      }
      case _ => parent.getFieldChooser(FieldList)
    }
  }

  def publishFieldStateChange(field:Field, position: Int, from:FieldChooserType): Unit = {
    if (!horizontal && contains(field)) {
      return
    }
    model.publishFieldStateChange(field, position, from, fieldChooserType)
  }

  def publishFieldStateChange(field:Field, newColumnStructure:ColumnStructure, from:FieldChooserType) {
    model.publishFieldStateChange(field, newColumnStructure, from)
  }

  def contains(field:Field) = fieldList.contains(field)
  def positionOfField(field: GuiFieldComponent) = fieldList.position(field.props.field)
  def getFieldName(index: Int) = fieldList.get(index).name
  def getNumberOfFields = fieldList.size
  def getGUIField(pos:Int) = guiFields(pos)

  def updateLayout(parentRevalidateRequired:Boolean = false) {
    removeAll
    if (!componentHolders.isEmpty) {
      if (fieldChooserType == Rows) setAlignment("al right")
      for (componentHolder <- componentHolders) {
        add(componentHolder.component, componentHolder.constraints)
      }
    } else {
      if (fieldChooserType == Rows) setAlignment("al left bottom")
      val label = new JLabel(defaultText) {
        setFont(GuiFieldFont)
      }
      val componentForSize = TempGuiFieldNamePanel(defaultText)
      label.setPreferredSize(componentForSize.preferredSize)
      label.setMinimumSize(componentForSize.preferredSize)
      label.setEnabled(false)
      add(label)
    }
    if (parentRevalidateRequired) {
      parent.revalidate
      parent.repaint
    } else {
      revalidate
      repaint()
    }
  }
}

