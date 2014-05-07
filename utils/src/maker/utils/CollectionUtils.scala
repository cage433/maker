package maker.utils

import maker.utils.RichIterable._
import maker.utils.RichString._
import java.io.File

object CollectionUtils{
  def filterOnType[T](collection : scala.collection.Traversable[_])(implicit m : Manifest[T]) = {
    collection.filter(m.erasure.isInstance(_)).map(_.asInstanceOf[T])
  }
  def reportSetDifference[A](set1Name : String, set2Name : String, set1 : Set[A], set2 : Set[A], tableWidth : Int = 1){
    def reportDiff(set1Name : String, set2Name : String, set1 : Set[A], set2 : Set[A]){
      val d1 = set1.filterNot(set2)
      if (d1.isEmpty)
        println(set1Name + " is contained in " + set2Name)
      else {
        println("Contained in " + set1Name + " but not in " + set2Name)
        println(d1.asTable(tableWidth).indent("  "))
      }
    }
    reportDiff(set1Name, set2Name, set1, set2)
    reportDiff(set2Name, set1Name, set2, set1)

  }
  def assertSetsIdentical[A](msg : String, set1Name : String, set2Name : String, set1 : Set[A], set2 : Set[A], tableWidth : Int = 1){
    if (set1 != set2){
      println(msg)
      reportSetDifference(set1Name : String, set2Name, set1, set2, tableWidth)
      throw new Exception("Sets not identical")
    }
  }
  def classFileToSource(sourceToClass : Map[File, Set[File]]) : Map[File, File] = {
    sourceToClass.flatMap{
      case (sourceFile, classFiles) => 
        classFiles.map((_, sourceFile))
    }.toMap
  }
}

