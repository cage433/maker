package starling.pivot.model

/**
 * Defines a set of distinct strings and current, 'selected' value (if any)
 */
class StringSet(values: Set[String]) {
  // select the first entity if there is one: otherwise none
  private var selectedValue: Option[String] = if(values.isEmpty) None else Some(values.toList.head)

  def select(value: AnyRef) {
    require(values.contains(value.toString), "Value "+value+" is not one of the available options.")
    selectedValue = Some(value.toString)
  }

  def getValues: Set[String] = values
  def getSelected: Option[AnyRef] = selectedValue
}