package starling.curves

import starling.quantity.Quantity
import starling.utils.cache.ThreadSafeCachingProxyInstance


case class ShiftedEnvironments(
  env : Environment,
  downEnv : Environment,
  upEnv : Environment,
  dP : Quantity
) 
