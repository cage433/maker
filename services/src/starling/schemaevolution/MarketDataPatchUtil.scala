package starling.schemaevolution

import starling.db.DBWriter
import starling.dbx.QueryBuilder._
import starling.marketdata.MarketDataTypeName
import starling.utils.ImplicitConversions._
import starling.richdb.RichDB


case class MarketDataPatchUtil(db: RichDB, writer: DBWriter) {
  def deleteMarketData(removeOrphaned: Boolean, types: MarketDataTypeName*): Unit =
    deleteMarketData(removeOrphaned, types.map(_.name).toList)

  def deleteMarketData(removeOrphaned: Boolean = true, typeNames: scala.List[String]) {
    writer.update("DELETE FROM MarketDataValue WHERE extendedKey IN (" +
      "SELECT DISTINCT id FROM MarketDataExtendedKey WHERE marketDataType IN (%s))" % typeNames.map(_.quote).mkString(", "))

    writer.delete("MarketDataExtendedKey", "marketDataType" in typeNames)

    if (removeOrphaned) removeOrphanedMarketDataValueKeys
  }

  def removeOrphanedMarketDataValueKeys {
    // a more efficient alternative could be to query for them before removing the MarketDataValues, but that could result in a large 'in clause'
    writer.update("DELETE FROM MarketDataValueKey WHERE id NOT IN (SELECT DISTINCT valueKey FROM MarketDataValue)")
  }

  def removeOrphanedMarketDataValues {
    writer.delete("MarketDataValue", "extendedKey" in(danglingExtendedKeys))
    writer.delete("MarketDataValue", "valueKey" in(danglingValueKeys))
  }

  private def danglingExtendedKeys = queryIds("MarketDataValue", "extendedKey").diff(queryIds("MarketDataExtendedKey", "id"))
  private def danglingValueKeys = queryIds("MarketDataValue", "valueKey").diff(queryIds("MarketDataValueKey", "id"))

  private def queryIds(table: String, column: String): Set[Int] =
    db.queryWithResult(select("DISTINCT " + column) from(table)) { _.getInt(column) }.toSet
}