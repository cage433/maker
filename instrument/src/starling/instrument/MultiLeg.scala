package starling.instrument

/**
 * Indicates a trade can be, but isn't necessarily, made up of component parts. For example a futures spread is made
 * up of 2 futures.
 */
trait MultiLeg {
  /**
   * List of underlying tradeables that make up this multi-leg trade. For example a future spread would return
   * two futures.
   * If called on a trade that isn't actually multi-leg then that trade returns itself.
   */
  def legs: List[Tradeable]
}