package starling.pivot.view.swing

import fieldchoosers.DropTarget
import starling.pivot._
import model.PivotTableModel
import swing.Swing._
import starling.pivot.FieldChooserType._
import collection.mutable.ListBuffer
import scala.{Right, Left}
import java.awt.{Point, Rectangle, Dimension, Color}
import swing.event._
import swing.Label
import starling.gui.GuiUtils

object ColumnDropPanel {
  def prefSize(text:String) = TempGuiFieldNamePanel(text).preferredSize
}

case class ColumnDropPanel(fieldOrColumnStructure:FieldOrColumnStructure, position:Position.Position) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  border = LineBorder(Color.RED)
  preferredSize = new Dimension(15,15)
  minimumSize = preferredSize
  visible = false

  reactions += {
    case MouseEntered(_,_,_) => {
      opaque = true
      repaint()
    }
    case MouseExited(_,_,_) => {
      opaque = false
      repaint()
    }
    case MouseClicked(_,_,_,_,_) => println(position + " : " + fieldOrColumnStructure)
  }
  listenTo(mouse.moves, mouse.clicks)
}

class FieldComponent(field:FieldAndIsMeasure, guiFieldsMap:Map[Field, GuiFieldComponent]) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  add(guiFieldsMap(field.field), "push,grow")
}

class ColumnTreeComponent(tree:ColumnTree, guiFieldsMap:Map[Field, GuiFieldComponent], dropPanels:ListBuffer[ColumnDropPanel]) extends MigPanel("insets 0, gap 0px", "[p][fill,grow][p]") {
  opaque = false

  private val topDropPanel = new ColumnDropPanel(tree.fieldOrColumnStructure, Position.Top)
  private val leftDropPanel = new ColumnDropPanel(tree.fieldOrColumnStructure, Position.Left)
  private val rightDropPanel = new ColumnDropPanel(tree.fieldOrColumnStructure, Position.Right)
  private val bottomDropPanel = new ColumnDropPanel(tree.fieldOrColumnStructure, Position.Bottom)
  dropPanels += topDropPanel
  dropPanels += leftDropPanel
  dropPanels += rightDropPanel
  dropPanels += bottomDropPanel

  add(topDropPanel, "skip 1, growx, hidemode 2, wrap")
  add(leftDropPanel, "growy, hidemode 2")

  tree.fieldOrColumnStructure.value match {
    case Left(f) => {
      add(new FieldComponent(f, guiFieldsMap), "push,grow")
    }
    case Right(cs) => {
      add(new ColumnStructureComponent(cs, guiFieldsMap, dropPanels), "push,grow")
    }
  }

  add(rightDropPanel, "growy, hidemode 2, wrap")
  val extraConstraints = if (tree.childStructure.trees.isEmpty) "" else ",wrap"
  add(bottomDropPanel, "growx, hidemode 2, skip 1" + extraConstraints)

  if (tree.childStructure.trees.nonEmpty) {
    add(new ColumnStructureComponent(tree.childStructure, guiFieldsMap, dropPanels), "spanx,push,grow")
  }
}

class ColumnStructureComponent(columnStructure:ColumnStructure, guiFieldsMap:Map[Field, GuiFieldComponent], dropPanels:ListBuffer[ColumnDropPanel]) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  columnStructure.trees.foreach(tree => {
    add(new ColumnTreeComponent(tree, guiFieldsMap, dropPanels), "push,grow")
  })
}


case class ColumnAndMeasureComponent(model:PivotTableModel, otherLayoutInfo:OtherLayoutInfo,
                        viewUI:PivotTableViewUI, tableView:PivotTableView) extends MigPanel("insets 0") with DropTarget {
  opaque = false
  private val cs = model.columns
  
  private def switchMeasureField(field:Field, from:FieldChooserType) {
    val newCS = model.columns.flipIsData(field)
    publishFieldStateChange(field, newCS, from)
  }
  private def publishFieldStateChange(field:Field, newColumnStructure:ColumnStructure, from:FieldChooserType) {
    model.publishFieldStateChange(field, newColumnStructure, from)
  }
  private def subTotalSubTotalToggle(field:Field, from:FieldChooserType) {
    tableView.publish(SubTotalToggled(field, from))
  }
  private val dropPanels = new ListBuffer[ColumnDropPanel]()

  private val fields = cs.allFields
  private val guiFieldsMap = Map() ++ fields.map(field => {
    val shouldShowDepthPanel = model.treeDetails.maxTreeDepths.getOrElse(field, 0) > 1
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = true
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = cs.measureFields.contains(field)
    val realMeasureField = model.isMeasureField(field)
    val subTotalToggleVisible = {
      otherLayoutInfo.totals.columnSubTotals
    }
    val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField,
      model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => switchMeasureField(_field, from), filterData, transformData, otherLayoutInfo,
      (_field, from) => subTotalSubTotalToggle(_field, from), subTotalToggleVisible, viewUI, tableView)
    (field -> new GuiFieldComponent(props))
  })

  if (fields.isEmpty) {
    val text = "Drop Column and Measure Fields Here"
    val prefSize = ColumnDropPanel.prefSize(text)
    val l = new Label(text) {
      font = GuiUtils.GuiFieldFont
      preferredSize = prefSize
      minimumSize = prefSize
      enabled = false
    }
    add(l)
  } else {
    val comp = new ColumnTreeComponent(ColumnTree(FieldOrColumnStructure(cs), ColumnStructure.Null), guiFieldsMap, dropPanels)
    add(comp, "pushy,growy")
  }

  def scrolling() {}

  def fieldChooserType = FieldChooserType.Columns
  def dropBounds(draggedField:Field) = {
    if (fields.isEmpty) {
      val screenPoint = peer.getLocationOnScreen
      new Rectangle(screenPoint.x, screenPoint.y, size.width, size.height) :: Nil
    } else {
      dropBoundsAndPanels(draggedField).map{case (bound, panel) => bound}
    }
  }

  def show(draggedField:Field) {
    dropPanels.foreach(_.visible = true)
    tableView.updateColumnAndMeasureScrollPane(true)
  }
  def hide() {
    dropPanels.foreach(_.visible = false)
    tableView.updateColumnAndMeasureScrollPane(true)
  }
  def reset() {guiFieldsMap.values.foreach(_.namePanel.reset())}

  private def filteredDropPanels(field:Field) = {
    dropPanels.toList.filterNot(p => {
      p.fieldOrColumnStructure.value match {
        case Left(f) => f.field == field
        case Right(_) => false
      }
    })
  }
  private def dropBoundsAndPanels(field:Field) = {
    filteredDropPanels(field).map(dp => {
      val screenPoint = dp.peer.getLocationOnScreen
      (new Rectangle(screenPoint.x, screenPoint.y, dp.size.width, dp.size.height), dp)
    })
  }

  def newColumnStructure(screenPoint:Point, field:Field) = {
    val measureField = model.isMeasureField(field) || cs.measureFields.contains(field)
    if (fields.isEmpty) {
      ColumnStructure(field, measureField)
    } else {
      val (_, panel) = dropBoundsAndPanels(field).find{case (bound, panel) => bound.contains(screenPoint)}.get
      val fieldOrColumnStructure = panel.fieldOrColumnStructure
      val pos = panel.position
      (if (cs.contains(field)) {
        cs.remove(field)
      } else {
        cs
      }).add(field, measureField, fieldOrColumnStructure, pos)
    }
  }
}

case class SubTotalToggled(field:Field, location:FieldChooserType) extends Event