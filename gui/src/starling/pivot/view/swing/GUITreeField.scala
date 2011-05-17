package starling.pivot.view.swing

import net.miginfocom.swing.MigLayout
import collection.mutable.ListBuffer
import javax.swing.{JLabel, JPanel}
import java.awt.Dimension
import starling.gui.GuiUtils
import starling.pivot._
import swing.Label

object MigTree {
  trait Command
  trait Constraint extends Command
  case class SpanX(n : Int) extends Constraint { override def toString = "spanx " + n }
  case object Wrap extends Constraint { override def toString = "wrap" }
  case object SpanY extends Constraint { override def toString = "spany" }
  case class Add[T](field : T, constraints : List[Constraint] = List()) extends Command {
    def addConstraint(c : Constraint) = Add(field, c :: constraints)
    def addConstraints(c : List[Constraint]) = Add(field, c ::: constraints)
  }

  def generateCommands[T <: HasChildren[T]](column:T) = {
    def width(css:Seq[T]) : Int = css match {
      case Nil => 1
      case _ => css.foldLeft(0)((sum, elem) => sum + width(elem.children))
    }

    def recurse(css:Seq[T]) : List[Command] = {
      val next = css.flatMap(_.children)
      val commands = if (!next.isEmpty) recurse(next) else Nil
      val thisRowCommands = Wrap :: css.reverse.flatMap(col => {
        (if (col.children.isEmpty) SpanY else SpanX(width(col.children))) :: Add(col) :: Nil
      }).toList
      commands ::: thisRowCommands
    }

    def gatherConstraints(state:(List[Constraint], List[Add[T]]), cmd:Command) : (List[Constraint], List[Add[T]]) = {
      val (cons, cmds) = state
      cmd match {
        case c : Constraint => (c :: cons, cmds)
        case a : Add[_] => (List(), a.asInstanceOf[Add[T]].addConstraints(cons) :: cmds)
      }
    }

//    val allCommands = recurse(column.children)
    val allCommands = recurse(List(column))
    val finalCommands = allCommands.foldLeft((List[Constraint](), List[Add[T]]()))(gatherConstraints)._2

    finalCommands
  }
}

import MigTree._

case class GUITreeField(column:ColumnStructure, guiFieldsMap:Map[Field, GuiFieldComponent], showFilter:Boolean, defaultText:String, parent:PivotTableView) {
  private val components = new ListBuffer[List[Add[ColumnTree]]]
  private val allGuiFields = guiFieldsMap.values.toSet

  def containsGuiField(guiField:GuiFieldComponent) = allGuiFields.contains(guiField)
  
  def populateComponents(columnStructure:ColumnStructure) {
    components.clear()
//    components ++= generateCommands(columnStructure.oldStyle)
    components ++= columnStructure.trees.map(generateCommands(_))
  }

  def resetComponents() {
//    populateComponents(column)
  }

  def resetImageState() {
//    allGuiFields.foreach(_.namePanel.resetImageState)
  }

  def reset() {
    /*resetComponents()
    updateLayout()*/
  }

  val mainComponent = new MigPanel("insets 0, gap 0px") {
    opaque = false

    column.trees.foreach(tree => {
      
    })

  }.peer




  def updateLayout(tempGuiComponent:Option[(TempGuiFieldNamePanel, Option[GuiFieldComponent], Position.Position)] = None) {
    /*mainComponent.removeAll()
    if (components.nonEmpty) {
      components.foreach(a => {
        a.foreach(b => {
          b.field.fieldOrColumnStructure.value match {
            case Left(f) => {
              val comp = if (guiFieldsMap.contains(f.field)) {
                guiFieldsMap(f.field).peer
              } else {
                tempGuiComponent.get._1.peer
              }
              mainComponent.add(comp, b.constraints.mkString(",") + ",grow")
            }
            case Right(cs) => {
              val cmds = cs.trees.map(generateCommands(_))

            }
          }
        })
      })
    } else {
      val label = new JLabel(defaultText) {
        setFont(GuiUtils.GuiFieldFont)
      }
      val componentForSize = TempGuiFieldNamePanel(defaultText)
      label.setPreferredSize(new Dimension(componentForSize.preferredSize.width + 50, componentForSize.preferredSize.height))
      label.setMinimumSize(new Dimension(10, componentForSize.preferredSize.height))
      label.setEnabled(false)
      mainComponent.add(label)
    }
    resetDropBounds()
    tempGuiComponent match {
      case None =>
      case Some(tup) => {
        val (_,maybeGF,pos) = tup
        maybeGF match {
          case None =>
          case Some(gf) => {
            gf.setDrawDropBounds(true, pos)
          }
        }
      }
    }
    mainComponent.revalidate()
    mainComponent.repaint()
    parent.updateColumnAndMeasureScrollPane(false)*/
  }

  private def resetDropBounds() {allGuiFields.foreach(_.setDrawDropBounds(false, Position.Other))}
  reset()
}