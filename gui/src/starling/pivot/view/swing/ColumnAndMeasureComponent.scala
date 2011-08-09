package starling.pivot.view.swing

import fieldchoosers.{EmptyDropLabel, DropPanel, DropTarget}
import starling.pivot._
import model.{EditableInfo, PivotTableModel}
import starling.pivot.FieldChooserType._
import collection.mutable.ListBuffer
import java.awt.{Point, Rectangle, Dimension, Graphics2D, RenderingHints}
import swing.event._
import starling.gui.GuiUtils
import scala.{Right, Left}

object ColumnDropPanel {
  def prefSize(text:String) = new TempGuiFieldNamePanel(text).preferredSize
  def apply(fieldOrColumnStructure:FieldOrColumnStructure, position:Position.Position) = new ColumnDropPanel(fieldOrColumnStructure, position)
}

class ColumnDropPanel(val fieldOrColumnStructure:FieldOrColumnStructure, val position:Position.Position) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  border = DropPanel.NormalBorder
  preferredSize = new Dimension(15,15)
  minimumSize = preferredSize
  visible = false

  private var mouseIn = false
  private var delayReset0 = false
  def delayReset() {delayReset0 = true}
  private var forceTintedPaint0 = false
  def forceTintedPaint() {
    visible = true
    border = DropPanel.OverBorder
    forceTintedPaint0 = true
  }

  reactions += {
    case MouseEntered(_,_,_) => {
      border = DropPanel.OverBorder
      mouseIn = true
      repaint()
    }
    case MouseExited(_,_,_) => {
      if (delayReset0) {
        delayReset0 = false
      } else {
        reset()
      }
    }
  }
  listenTo(mouse.moves)

  def reset() {
    forceTintedPaint0 = false
    mouseIn = false
    border = DropPanel.NormalBorder
    repaint()
  }

  override def visible_=(b:Boolean) {
    super.visible = b
    if (!b) {
      reset()
    }
  }

  override protected def paintComponent(g:Graphics2D) {
    if (!mouseIn && !forceTintedPaint0) {
      super.paintComponent(g)
    } else {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setColor(GuiUtils.DropPanelOverColour)
      g.fillRoundRect(1,1,size.width-2, size.height-2,GuiUtils.GuiFieldArc,GuiUtils.GuiFieldArc)
    }
  }
}

class FieldComponent(field:FieldAndIsMeasure, fieldOrCS:FieldOrColumnStructure,
                     guiFieldsMap:Map[Field, GuiFieldComponent], dropPanels:ListBuffer[ColumnDropPanel],
                     bottomDropPanel:Boolean) extends MigPanel("insets 0, gap 0px") {
  opaque = false

  {
    val topDropPanel = new ColumnDropPanel(fieldOrCS, Position.Top)
    val leftDropPanel = new ColumnDropPanel(fieldOrCS, Position.Left)
    val rightDropPanel = new ColumnDropPanel(fieldOrCS, Position.Right)
    dropPanels += topDropPanel
    dropPanels += leftDropPanel
    dropPanels += rightDropPanel

    add(topDropPanel, "skip 1, growx, hidemode 2, wrap")
    add(leftDropPanel, "growy, hidemode 2")
    add(guiFieldsMap(field.field), "push,grow")
    add(rightDropPanel, "growy, hidemode 2")
//    if (bottomDropPanel) {
    if (true) {
      val bottomDropPanel = new ColumnDropPanel(fieldOrCS, Position.Bottom)
      dropPanels += bottomDropPanel
      add(bottomDropPanel, "newline, growx, hidemode 2, skip 1")
    }
  }
}

class ColumnTreeComponent(tree:ColumnTree, guiFieldsMap:Map[Field, GuiFieldComponent],
                          dropPanels:ListBuffer[ColumnDropPanel]) extends MigPanel("insets 0, gap 0px") {
  opaque = false

  val bottomDropPanel = tree.childStructure.trees.size != 1
  tree.fieldOrColumnStructure.value match {
    case Left(f) => {
      add(new FieldComponent(f, tree.fieldOrColumnStructure, guiFieldsMap, dropPanels, bottomDropPanel), "push,grow")
    }
    case Right(cs) => {
      add(new ColumnStructureComponent(cs, guiFieldsMap, dropPanels, bottomDropPanel), "push,grow")
    }
  }
  
  if (tree.childStructure.trees.nonEmpty) {
    add(new ColumnStructureComponent(tree.childStructure, guiFieldsMap, dropPanels, true), "newline,spanx,push,grow")
  }
}

class ColumnStructureComponent(columnStructure:ColumnTrees, guiFieldsMap:Map[Field, GuiFieldComponent],
                               dropPanels:ListBuffer[ColumnDropPanel], showBottomDropPanel:Boolean) extends MigPanel("insets 0, gap 0px") {
  opaque = false

  {
//    val showDropPanels = columnStructure.trees.size > 1 || (columnStructure.trees.size == 1 && columnStructure.trees.head.childStructure.trees.nonEmpty)
    val showDropPanels = true
    if (showDropPanels) {
      val fieldOrCS = FieldOrColumnStructure(columnStructure)
      val topDropPanel = new ColumnDropPanel(fieldOrCS, Position.Top)
      val leftDropPanel = new ColumnDropPanel(fieldOrCS, Position.Left)
      val rightDropPanel = new ColumnDropPanel(fieldOrCS, Position.Right)
      dropPanels += topDropPanel
      dropPanels += leftDropPanel
      dropPanels += rightDropPanel // This has got to be the 3rd drop panel added for the fieldGoingToBeAddedToTheEnd method at the bottom of this file.
      val holderPanel = new MigPanel("insets 0, gap 0px", "[p][fill,grow][p]") {
        opaque = false
        add(topDropPanel, "skip 1, growx, hidemode 2, wrap")
        add(leftDropPanel, "growy, hidemode 2")
        val splitConstraint = ",split " + columnStructure.trees.size
        columnStructure.trees.foreach(tree => {
          add(new ColumnTreeComponent(tree, guiFieldsMap, dropPanels), "gap 0px,push,grow" + splitConstraint)
        })
        add(rightDropPanel, "growy, hidemode 2")
//        if (showBottomDropPanel) {
        if (true) {
          val bottomDropPanel = new ColumnDropPanel(fieldOrCS, Position.Bottom)
          dropPanels += bottomDropPanel
          add(bottomDropPanel, "newline,growx, hidemode 2, skip 1")
        }
      }
      add(holderPanel, "push,grow")
    } else {
      columnStructure.trees.foreach(tree => {
        add(new ColumnTreeComponent(tree, guiFieldsMap, dropPanels), "push,grow")
      })
    }
  }
}

class ColumnAndMeasureComponent(model:PivotTableModel, otherLayoutInfo:OtherLayoutInfo,
                        viewUI:PivotTableViewUI, tableView:PivotTableView, editableInfo:Option[EditableInfo]) extends MigPanel("insets 0") with DropTarget {
  opaque = false
  private val cs = model.columns
  
  private def switchMeasureField(field:Field, from:FieldChooserType) {
    val newCS = model.columns.flipIsData(field)
    publishFieldStateChange(field, newCS, from)
  }
  private def publishFieldStateChange(field:Field, newColumnStructure:ColumnTrees, from:FieldChooserType) {
    model.publishFieldStateChange(field, newColumnStructure, from)
  }
  private def subTotalSubTotalToggle(field:Field, from:FieldChooserType) {
    tableView.publish(SubTotalToggled(field, from))
  }
  private val dropPanels = new ListBuffer[ColumnDropPanel]()

  private val bottomNonMeasureFields = cs.bottomNonMeasureFields
  private val measureFieldsOnBottomRow = cs.measureFieldsOnBottomRow

  private val fields = cs.allFields
  private val guiFieldsMap = Map() ++ fields.map(field => {
    val shouldShowDepthPanel = model.treeDetails.maxTreeDepths.getOrElse(field, 0) > 1
    val filterData = FilterData(model.possibleValuesAndSelection(field), (_field, selection) => model.setFilter(_field, selection))
    val showOther = true
    val transformData = TransformData(showOther, model.transforms(field), (_field, transform) => model.setTransform(_field, transform))
    val currentlyActingAsMeasure = cs.measureFields.contains(field)
    val realMeasureField = model.isMeasureField(field)
    val subTotalToggleVisible = {
      val subTotalsOn = otherLayoutInfo.totals.columnSubTotals
      val isNormalFieldWithNoMeasureFieldBelow = bottomNonMeasureFields.contains(field)
      val isMeasureFieldWithNothingBelow = measureFieldsOnBottomRow.contains(field)
      val measure = (realMeasureField || currentlyActingAsMeasure)
      val isNormalFieldWithOnlyMeasureChild = {
        if (!measure) {
          cs.hasSingleMeasureChild(field)
        } else {
          false
        }
      }

      subTotalsOn && !isNormalFieldWithNoMeasureFieldBelow && !isMeasureFieldWithNothingBelow && !isNormalFieldWithOnlyMeasureChild
    }
    val props = GuiFieldComponentProps(field, fieldChooserType, shouldShowDepthPanel,
      currentlyActingAsMeasure, realMeasureField,
      model.treeDetails, (_field,depth) => {model.setDepth(_field,depth)},
      (_field, from) => switchMeasureField(_field, from), filterData, transformData, otherLayoutInfo,
      (_field, from) => subTotalSubTotalToggle(_field, from), subTotalToggleVisible, viewUI, tableView, editableInfo)
    (field -> new GuiFieldComponent(props))
  })

  private val blankDropLabel = if (fields.isEmpty) {
    Some(new EmptyDropLabel("Drop Column and Measure Fields Here", tableView))
  } else {
    None
  }
  blankDropLabel match {
    case Some(l) => add(l)
    case _ => add(new ColumnStructureComponent(cs, guiFieldsMap, dropPanels, true), "pushy,growy")
  }

  def scrolling() {
    // If the mouse is currently over the column area, reset the display state.
    val mousePos = peer.getMousePosition(true)
    if (mousePos != null) {
      guiFieldsMap.map(_._2.namePanel.showComponent())
      viewUI.resetImageProperties()
    }
  }

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
    val draggedFieldIsMeasure = model.isMeasureField(draggedField) || cs.measureFields.contains(draggedField)
    val dropPanelToResultMap = Map() ++ dropPanels.map(dp => {
      (dp,cs.add(draggedField, draggedFieldIsMeasure, dp.fieldOrColumnStructure, dp.position))
    })

    val grouped:Map[ColumnTrees, Map[ColumnDropPanel, ColumnTrees]] = dropPanelToResultMap.groupBy{case (p,t) => t}

    grouped.foreach{case (trees, map) => {
      val panels = map.keySet
      if (panels.exists(p => p.position == Position.Left || p.position == Position.Right)) {
        panels.foreach(_.visible = true)
      } else {
        val closeByPanels = panels.filter(p => {
          p.fieldOrColumnStructure.value match {
            case Left(f) if f.field == draggedField && (p.position == Position.Top || p.position == Position.Bottom) => true
            case _ => false
          }
        })
        val special = closeByPanels.nonEmpty
        if (special) {
          // Ensure the panels to the top and to the bottom of the dragged field are shown, but nothing else.
          closeByPanels.foreach(_.visible = true)
        } else {
          panels.find(_.position == Position.Top) match {
            case Some(p) => p.visible = true
            case _ => panels.head.visible = true
          }
        }
      }
    }}

    tableView.updateColumnAndMeasureScrollPane(true)
  }
  def hide() {
    dropPanels.foreach(_.visible = false)
    tableView.updateColumnAndMeasureScrollPane(true)
  }
  def reset() {
    blankDropLabel.foreach(_.reset())
    dropPanels.foreach(dp => {
      dp.visible = false
      dp.reset()
    })
    guiFieldsMap.values.foreach(_.namePanel.reset())
    tableView.updateColumnAndMeasureScrollPane(true)
  }

  private def filteredDropPanels(field:Field) = {
    dropPanels.toList.filter(p => p.visible)
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
      blankDropLabel.foreach(_.delayReset())
      ColumnTrees(field, measureField)
    } else {
      val (_, panel) = dropBoundsAndPanels(field).find{case (bound, _) => bound.contains(screenPoint)}.get
      panel.delayReset()
      val fieldOrColumnStructure = panel.fieldOrColumnStructure
      val pos = panel.position
      val tmpField = Field("fhsdvbhsvuilh")
      cs.add(tmpField, measureField, fieldOrColumnStructure, pos).remove(field).rename(tmpField, field.name)
    }
  }

  def fieldGoingToBeAddedToTheEnd() {
    if (fields.isEmpty) {
      blankDropLabel.foreach(_.forceTintedPaint())
    } else {
      dropPanels(2).forceTintedPaint()
    }
  }
}

case class SubTotalToggled(field:Field, location:FieldChooserType) extends Event