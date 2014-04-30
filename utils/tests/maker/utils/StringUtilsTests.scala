package maker.utils

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import maker.utils.StringUtils._

class StringUtilsTests extends FunSuite with TableDrivenPropertyChecks{

  test("intellij distance"){
    val table = Table(
      ("matcher", "name", "should match")
      ,("AbCd", "AbsdCdk", Some(0))
      ,("AbCd", "AxCd", None)
      ,("Ax", "AxCd", Some(1))
      ,("A", "ACd", Some(1))
      ,("AC", "ACd", Some(0))
      ,("", "AbCd", None)
    )

    forAll(table){
      (matcher, name, shouldMatch) => 
        assert(intellijDistance(matcher, name) === shouldMatch)
    }
  }

  test("best intellij matches"){
    val table = Table(
      ("matcher", "names", "best")
      ,("Ab", List("Abc", "AbcDef", "Ab"), List("Ab", "Abc"))
      ,("AbD", List("Abc", "AbcDef", "Ab"), List("AbcDef"))
      ,("", List("Abc", "AbcDef", "Ab"), Nil)
      ,("AbD", List("bill.Abc", "fred.mike.AbcDef", "bob.Ab"), List("fred.mike.AbcDef"))
    )
    forAll(table){
      (matcher, names, best) => 
        assert(bestIntellijMatches(matcher, names) === best)
    }

  }
}
