package starling.curves

import starling.quantity.Quantity

abstract class DelegatingAtomicEnvironment(atomicEnv: AtomicEnvironment) extends AtomicEnvironment {
  def apply(key: AtomicDatumKey) = atomicEnv.apply(key)
  def shiftsCanBeIgnored = atomicEnv.shiftsCanBeIgnored
  def marketDay = atomicEnv.marketDay
}

case class NamingAtomicEnvironment(atomicEnv: AtomicEnvironment, prefix:String) extends DelegatingAtomicEnvironment(atomicEnv) {
  def setShiftsCanBeIgnored(canBeIgnored: Boolean) = copy(atomicEnv = atomicEnv.setShiftsCanBeIgnored(canBeIgnored))

  override def apply(key: AtomicDatumKey) = {
    val value = atomicEnv(key)
    value match {
      case q:Quantity => {
        val pointSuffix = key.point match {
          case None => ""
          case other => "." + other
        }
        val curveName = key match {
          case USDFXRateKey(ccy) => "USD per " + ccy.toString + " spot"
          case _ => key.curveKey.underlying
        }
        val name = curveName + pointSuffix
        q.named(name + (if (prefix.isEmpty) "" else "@" + prefix))
      }
      case other => other
    }
  }
}
