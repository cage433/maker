package starling.pivot.view.swing

import fieldchoosers.{DropPanel, DropTarget}
import starling.pivot._
import model.PivotTableModel
import starling.pivot.FieldChooserType._
import collection.mutable.ListBuffer
import java.awt.{Point, Rectangle, Dimension, Graphics2D, RenderingHints}
import swing.event._
import swing.Label
import starling.gui.GuiUtils
import scala.{Right, Left}

object ColumnDropPanel {
  def prefSize(text:String) = TempGuiFieldNamePanel(text).preferredSize
}

case class ColumnDropPanel(fieldOrColumnStructure:FieldOrColumnStructure, position:Position.Position) extends MigPanel("insets 0, gap 0px") {
  opaque = false
  border = DropPanel.NormalBorder
  preferredSize = new Dimension(15,15)
  minimumSize = preferredSize
  visible = false

  private var mouseIn = false
  private var valid0 = true
  def valid = valid0
  def valid_=(b:Boolean) {
    valid0 = b
    if (b) {
      border = DropPanel.NormalBorder
    } else {
      border = DropPanel.InvalidBorder
    }
  }

  reactions += {
    case MouseEntered(_,_,_) => {
      if (valid) {
        border = DropPanel.OverBorder
      } else {
        border = DropPanel.InvalidBorder
      }
      mouseIn = true
      repaint()
    }
    case MouseExited(_,_,_) => {
      reset()
    }
  }
  listenTo(mouse.moves)

  def reset() {
    if (valid0) {
      border = DropPanel.NormalBorder
    } else {
      border = DropPanel.InvalidBorder
    }
    mouseIn = false
    repaint()
  }

  override def visible_=(b:Boolean) {
    super.visible = b
    if (!b) {
      reset()
    }
  }

  override protected def paintComponent(g:Graphics2D) {
    if (!mouseIn) {
      super.paintComponent(g)
    } else {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      if (valid) {
        g.setColor(GuiUtils.DropPanelOverColour)
      } else {
        g.setColor(GuiUtils.DropPanelOverColourInvalid)
      }
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
    if (bottomDropPanel) {
      val bottomDropPanel = new ColumnDropPanel(fieldOrCS, Position.Bottom)
      dropPanels += bottomDropPanel
      add(bottomDropPanel, "newline, growx, hidemode 2, skip 1")
    }
  }
}

class ColumnTreeComponent(tree:ColumnTree, guiFieldsMap:Map[Field, GuiFieldComponent],
                          dropPanels:ListBuffer[ColumnDropPanel]) extends MigPanel("insets 0, gap 0px") {
  opaque = false

  tree.fieldOrColumnStructure.value match {
    case Left(f) => {
      val bottomDropPanel = tree.childStructure.trees.size != 1
      add(new FieldComponent(f, tree.fieldOrColumnStructure, guiFieldsMap, dropPanels, bottomDropPanel), "push,grow")
    }
    case Right(cs) => {
      add(new ColumnStructureComponent(cs, guiFieldsMap, dropPanels), "push,grow")
    }
  }
  
  if (tree.childStructure.trees.nonEmpty) {
    add(new ColumnStructureComponent(tree.childStructure, guiFieldsMap, dropPanels), "newline,spanx,push,grow")
  }
}

class ColumnStructureComponent(columnStructure:ColumnStructure, guiFieldsMap:Map[Field, GuiFieldComponent],
                               dropPanels:ListBuffer[ColumnDropPanel]) extends MigPanel("insets 0, gap 0px") {
  opaque = false

  {
    val showDropPanels = columnStructure.trees.size > 1 || (columnStructure.trees.size == 1 && columnStructure.trees.head.childStructure.trees.nonEmpty)
    if (showDropPanels) {
      val fieldOrCS = FieldOrColumnStructure(columnStructure)
      val topDropPanel = new ColumnDropPanel(fieldOrCS, Position.Top)
      val leftDropPanel = new ColumnDropPanel(fieldOrCS, Position.Left)
      val rightDropPanel = new ColumnDropPanel(fieldOrCS, Position.Right)
      val bottomDropPanel = new ColumnDropPanel(fieldOrCS, Position.Bottom)
      dropPanels += topDropPanel
      dropPanels += leftDropPanel
      dropPanels += rightDropPanel
      dropPanels += bottomDropPanel
      val holderPanel = new MigPanel("insets 0, gap 0px", "[p][fill,grow][p]") {
        opaque = false
        add(topDropPanel, "skip 1, growx, hidemode 2, wrap")
        add(leftDropPanel, "growy, hidemode 2")
        val splitConstraint = ",split " + columnStructure.trees.size
        columnStructure.trees.foreach(tree => {
          add(new ColumnTreeComponent(tree, guiFieldsMap, dropPanels), "gap 0px,push,grow" + splitConstraint)
        })
        add(rightDropPanel, "growy, hidemode 2,wrap")
        add(bottomDropPanel, "growx, hidemode 2, skip 1")
      }
      add(holderPanel, "push,grow")
    } else {
      columnStructure.trees.foreach(tree => {
        add(new ColumnTreeComponent(tree, guiFieldsMap, dropPanels), "push,grow")
      })
    }
  }
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
    add(new ColumnStructureComponent(cs, guiFieldsMap, dropPanels), "pushy,growy")
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
    dropPanels.foreach(dp => {
      dp.fieldOrColumnStructure.value match {
        case Left(f) if f.field == draggedField => dp.valid = false
        case _ => dp.valid = true
      }
      dp.visible = true
    })
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
      val tmpField = Field("fhsdvbhsvuilh")
      cs.add(tmpField, measureField, fieldOrColumnStructure, pos).remove(field).rename(tmpField, field.name)
    }
  }
}

case class SubTotalToggled(field:Field, location:FieldChooserType) extends Event