package starling.loopyxl

import starling.utils.Log
import starling.utils.ImplicitConversions._
import MethodOwner._

trait MethodSource {
  def getMethods: List[DynamicMethod]
  def lookup(methodId: Int): DynamicMethod
}

class ReflectiveMethodSource(methodOwners: Traversable[MethodOwner]) extends MethodSource {
  def this(owners: AnyRef*) = this(owners.map(owner => new MethodOwner(owner)))

  private val dynamicMethodsMap = methodOwners.dynamicMethods.toMapWithKeys(_.id)
  Log.info("===")
  dynamicMethodsMap.values.foreach(method => Log.info(method.name))
  Log.info("===")

  def getMethods = dynamicMethodsMap.values.toList
  def lookup(methodId: Int) = dynamicMethodsMap.get(methodId).get
}