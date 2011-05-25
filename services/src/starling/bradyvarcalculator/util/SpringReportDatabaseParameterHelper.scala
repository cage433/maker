package starling.bradyvarcalculator.util

object SpringReportDatabaseParameterHelper {
  val AnyString = "ANY"
  val NoneString = "NONE"
  val UOMString = "UOM"

  def escapeAll(stringToEscape: String): String = {

    //If input is null return null otherwise escape the string
    if (stringToEscape == null) {
      null
    } else {

      //Find all Alls and double them
      stringToEscape.replaceAll(AnyString, AnyString + AnyString)
    }
  }

  def unEscapeAll(stringToUnescape: String): String = {

    //If input is null return null otherwise escape the string
    if (stringToUnescape == null) {
      null
    } else {

      //Find all double Alls and turn them into All
      stringToUnescape.replaceAll(AnyString + AnyString, AnyString)
    }
  }

  def escapeNone(stringToEscape: String): String = {

    //If input is null return null otherwise escape the string
    if (stringToEscape == null) {
      null
    } else {

      //Find all Alls and double them
      stringToEscape.replaceAll(NoneString, NoneString + NoneString)
    }
  }

  def unEscapeNone(stringToUnescape: String): String = {

    //If input is null return null otherwise escape the string
    if (stringToUnescape == null) {
      null
    } else {

      //Find all double Alls and turn them into All
      stringToUnescape.replaceAll(NoneString + NoneString, NoneString)
    }
  }

}