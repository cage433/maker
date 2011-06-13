package starling.pivot.view.swing.fieldchoosers

import starling.pivot.model.PivotTableModel
import collection.mutable.ListBuffer
import starling.pivot.view.swing._
import starling.pivot.FieldChooserType._
import net.miginfocom.swing.MigLayout
import starling.pivot.{Position, Field, OtherLayoutInfo}
import java.awt.{Point, Rectangle}
import scala.Some

class RowComponent(model:PivotTableModel,  otherLayoutInfo:OtherLayoutInfo, viewUI:PivotTableViewUI, tableView:PivotTableView)
        extends MigPanel("insets 1 1 1 " + (if (otherLayoutInfo.frozen) "1" else "0") + ", gap 0px") with DropTarget {
  opaque = false

  private val fields = model.getFields(Rows)
  private val layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  private val baseLayout = layoutManager.getLayoutConstraints
  private def setAlignment(alignment:String) {
    layoutManager.setLayoutConstraints(baseLayout + ", " + alignment)
  }

  private val guiFieldsMap = Map() ++ fields.fields.map(field => {
    val shouldShowDepthPanel = model.treeDetails.maxTreeDepths.getOrElse(field, 0) > 1
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = true
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = false
    val realMeasureField = model.isMeasureField(field)
    val subTotalToggleVisible = {
      if (fields.isEmpty) {
        false
      } else {
        otherLayoutInfo.totals.rowSubTotals && (fields.fields.last != field)
      }
    }
    val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField,
      model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => (), filterData, transformData, otherLayoutInfo,
      (_field, from) => subTotalSubTotalToggle(_field, from), subTotalToggleVisible, viewUI, tableView)
    (field -> new GuiFieldComponent(props))
  })

  private def subTotalSubTotalToggle(field:Field, from:FieldChooserType) {
    tableView.publish(SubTotalToggled(field, from))
  }

  private val dropPanels = new ListBuffer[DropPanel]()
  private val blankDropLabel = if (fields.isEmpty) {
    Some(EmptyDropLabel("Drop Row Fields Here", tableView))
  } else {
    None
  }
  blankDropLabel match {
    case Some(l) => {
      setAlignment("al left bottom")
      add(l)
    }
    case _ => {
      setAlignment("al right")
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
  }

  def fieldChooserType = Rows
  def dropBounds(draggedField:Field) = {
    if (fields.isEmpty) {
      val screenPoint = peer.getLocationOnScreen
      new Rectangle(screenPoint.x, screenPoint.y, size.width, size.height) :: Nil
    } else {
      dropBoundsAndPanels(draggedField).map(_._1)
    }
  }

  def show(draggedField:Field) {
    rowDropPanels(draggedField).foreach(_.visible = true)
  }
  def hide() {
    dropPanels.foreach(_.visible = false)
  }
  def reset() {
    blankDropLabel.foreach(_.reset())
    dropPanels.foreach(dp => {
      dp.visible = false
      dp.reset()
    })
    guiFieldsMap.values.foreach(_.namePanel.reset())
  }

  private def rowDropPanels(field:Field) = {
    dropPanels.toList.filterNot(dp => {dp.fieldAndPositions.map(_._1).contains(field)})
  }

  private def dropBoundsAndPanels(field:Field) = {
    rowDropPanels(field).map(dp => {
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

  def numberOfFields = fields.size
  def guiField(index:Int) = guiFieldsMap(fields.get(index))
}