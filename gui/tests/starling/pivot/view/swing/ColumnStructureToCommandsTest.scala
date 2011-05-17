package starling.pivot.view.swing

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.pivot.view.swing.MigTree._
import starling.pivot.ColumnTree._
import starling.pivot.FieldOrColumnStructure._
import starling.pivot.ColumnStructure._
import starling.pivot.PivotFieldsState._
import starling.pivot.Field._
import starling.pivot._

class ColumnStructureToCommandsTest extends TestNGSuite {
  @Test
  def commandsTest() {
    val cs1 = ColumnStructure(List(
      ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true)
    ))
    val cs = ColumnStructure(List(
      ColumnTree(FieldOrColumnStructure(Right(cs1)), ColumnStructure(Field("Product"), false, List()))
    ))



    val commands = cs.trees.map(generateCommands(_))


    println(commands)

    
  }
}