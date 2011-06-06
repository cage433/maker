package starling.pivot.view.swing.fieldchoosers

import starling.pivot.model.PivotTableModel
import starling.pivot.FieldChooserType.Filter
import starling.pivot.view.swing._
import swing.Label
import starling.pivot.{Position, Field, OtherLayoutInfo}
import collection.mutable.ListBuffer
import java.awt.{Point, Dimension, Color, Rectangle, Graphics2D, RenderingHints}
import starling.gui.{RoundedBorder, GuiUtils}
import swing.event.{MouseExited, MouseEntered}
import swing.Swing._

object DropPanel {
  val NormalBorder = RoundedBorder(Color.LIGHT_GRAY)
  val OverBorder = RoundedBorder(GuiUtils.GuiFieldFilterNumberColour)
  val InvalidBorder = RoundedBorder(Color.RED)
}

case class DropPanel(fieldAndPositions:List[(Field,Position.Position)]) extends MigPanel("insets 0, gap 0px") {
  import DropPanel._
  
  opaque = false
  visible = false
  border = NormalBorder
  preferredSize = new Dimension(15,15)
  minimumSize = preferredSize
  private var mouseIn = false

  reactions += {
    case MouseEntered(_,_,_) => {
      border = OverBorder
      mouseIn = true
      repaint()
    }
    case MouseExited(_,_,_) => {
      reset()
    }
  }
  listenTo(mouse.moves)

  def reset() {
    border = DropPanel.NormalBorder
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
      g.setColor(GuiUtils.DropPanelOverColour)
      g.fillRoundRect(1,1,size.width-2, size.height-2,GuiUtils.GuiFieldArc,GuiUtils.GuiFieldArc)
    }
  }
}

case class EmptyDropLabel(text0:String, view:PivotTableView) extends Label(text0) {
  import DropPanel._

  private val prefSize = ColumnDropPanel.prefSize(text)
  font = GuiUtils.GuiFieldFont
  preferredSize = prefSize
  minimumSize = prefSize
  enabled = false
  var mouseIn = false
  reactions += {
    case MouseEntered(_,_,_) if view.mouseDown && view.fieldBeingDragged => {
      mouseIn = true
      border = OverBorder
      repaint()
    }
    case MouseExited(_,_,_) => {
      reset()
    }
  }
  listenTo(mouse.moves)

  def reset() {
    mouseIn = false
    border = swing.Swing.EmptyBorder
    repaint()
  }

  override protected def paintComponent(g:Graphics2D) {
    if (!mouseIn) {
      super.paintComponent(g)
    } else {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setColor(GuiUtils.DropPanelOverColour)
      g.fillRoundRect(1,1,size.width-2, size.height-2,GuiUtils.GuiFieldArc,GuiUtils.GuiFieldArc)
    }
  }
}

class FilterComponent(model:PivotTableModel, otherLayoutInfo:OtherLayoutInfo,
                         viewUI:PivotTableViewUI, tableView:PivotTableView)
        extends MigPanel("insets 1, gap 0px") with DropTarget {
  opaque = false
  border = MatteBorder(0,0,1,0,GuiUtils.BorderColour)

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
      (_field, from) => (), subTotalToggleVisible, viewUI, tableView)
    (field -> new GuiFieldComponent(props))
  })

  private val dropPanels = new ListBuffer[DropPanel]()
  private val blankDropLabel = if (fields.isEmpty) {
    Some(EmptyDropLabel("Drop Filter Fields Here", tableView))
  } else {
    None
  }
  blankDropLabel match {
    case Some(l) => add(l)
    case _ => {
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
  def reset() {
    blankDropLabel.foreach(_.reset())
    dropPanels.foreach(dp => {
      dp.visible = false
      dp.reset()
    })
    guiFieldsMap.values.foreach(_.namePanel.reset())
  }

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