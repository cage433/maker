package starling.pivot.view.swing.fieldchoosers

import starling.pivot.model.PivotTableModel
import starling.pivot.FieldChooserType.Filter
import starling.pivot.view.swing._
import starling.gui.GuiUtils
import swing.Label
import starling.pivot.{Position, Field, OtherLayoutInfo}
import swing.Swing._
import collection.mutable.ListBuffer
import java.awt.{Point, Dimension, Color, Rectangle}

case class DropPanel(fieldAndPositions:List[(Field,Position.Position)]) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  visible = false
  border = LineBorder(Color.RED)
  preferredSize = new Dimension(15,15)
  minimumSize = preferredSize
}

class FilterComponent(model:PivotTableModel, otherLayoutInfo:OtherLayoutInfo,
                         viewUI:PivotTableViewUI, mainComponent:PivotTableView)
        extends MigPanel("insets 1, gap 0px") with DropTarget {
  opaque = false

  private val fields = model.getFields(Filter)

  private val guiFieldsMap = Map() ++ fields.fields.map(field => {
    val shouldShowDepthPanel = false
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = false
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = false
    val realMeasureField = model.isMeasureField(field)
    val subTotalToggleVisible = false
    val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField,
      model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => (), filterData, transformData, otherLayoutInfo,
      (_field, from) => (), subTotalToggleVisible, viewUI, mainComponent)
    (field -> new GuiFieldComponent(props))
  })

  private val dropPanels = new ListBuffer[DropPanel]()
  if (fields.isEmpty) {
    val text = "Drop Filter Fields Here"
    val prefSize = ColumnDropPanel.prefSize(text)
    val l = new Label(text) {
      font = GuiUtils.GuiFieldFont
      preferredSize = prefSize
      minimumSize = prefSize
      enabled = false
    }
    add(l)
  } else {
    fields.fields.zipWithIndex.foreach{case (f,i) => {
      if (i == 0) {
        val dropPanel = DropPanel((f, Position.Left) :: Nil)
        dropPanels += dropPanel
        add(dropPanel, "grow, hidemode 3")
      }
      add(guiFieldsMap(f))
      if ((i + 1) < fields.fields.size) {
        val dropPanel = DropPanel((f, Position.Right) :: (fields.fields(i + 1), Position.Left) :: Nil)
        dropPanels += dropPanel
        add(dropPanel, "grow, hidemode 3")
      } else {
        val dropPanel = DropPanel((f, Position.Right) :: Nil)
        dropPanels += dropPanel
        add(dropPanel, "grow, hidemode 3")
      }
    }}
  }

  def fieldChooserType = Filter
  def dropBounds(draggedField:Field) = {
    if (fields.isEmpty) {
      val screenPoint = peer.getLocationOnScreen
      new Rectangle(screenPoint.x, screenPoint.y, size.width, size.height) :: Nil
    } else {
      dropBoundsAndPanels(draggedField).map(_._1)
    }
  }

  def show(draggedField:Field) {
    filteredDropPanels(draggedField).foreach(_.visible = true)
  }
  def hide() {
    dropPanels.foreach(_.visible = false)
  }
  def reset() {guiFieldsMap.values.foreach(_.namePanel.reset())}

  private def filteredDropPanels(field:Field) = {
    dropPanels.toList.filterNot(dp => {dp.fieldAndPositions.map(_._1).contains(field)})
  }

  private def dropBoundsAndPanels(field:Field) = {
    filteredDropPanels(field).map(dp => {
      val screenPoint = dp.peer.getLocationOnScreen
      (new Rectangle(screenPoint.x, screenPoint.y, dp.size.width, dp.size.height), dp)
    })
  }

  def indexOfDrop(screenPoint:Point, field:Field) = {
    if (fields.isEmpty) {
      0
    } else {
      val (_, panel) = dropBoundsAndPanels(field).find{case (bound, _) => bound.contains(screenPoint)}.get
      val (nextToField, pos) = panel.fieldAndPositions.head
      val fieldPos = fields.position(nextToField)
      pos match {
        case Position.Left => math.max(fieldPos - 1, 0)
        case Position.Right => fieldPos + 1
      }
    }
  }
}