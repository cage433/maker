package starling.curves

import starling.quantity.Quantity


case class ShiftedEnvironments(
  env : Environment,
  downEnv : Environment,
  upEnv : Environment,
  dP : Quantity
) 
