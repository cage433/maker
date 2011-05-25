package starling.tradestore.eai

import starling.eai.EAIStrategyDB
import starling.pivot.PivotTreePath
import starling.utils.ImplicitConversions._


class ExternalSortIndexPivotTreePathOrdering(eaiStrategyDB : EAIStrategyDB) extends Ordering[PivotTreePath] {
  def compare(x: PivotTreePath, y: PivotTreePath) = {
    val xIds = eaiStrategyDB.getIds(x.path).map(_.id).zip(x.path).toIterable
    val yIds = eaiStrategyDB.getIds(y.path).map(_.id).zip(y.path).toIterable

    xIds.compareTo(yIds)
  }
}

