package starling.pivot.model

case class CollapsedElement(path:List[Any])

case class CollapsedState(elements:List[CollapsedElement]) {
  def addElement(element:CollapsedElement) = {
    CollapsedState(element :: elements)
  }
  def removeElement(element:CollapsedElement) = {
    CollapsedState(elements.filterNot(_ == element))
  }
  def collapsed(path:List[Any]) = {
    elements.contains(CollapsedElement(path))
  }
}
object CollapsedState {
  val None = new CollapsedState(List())
}