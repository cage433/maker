package starling.marketdata

import starling.daterange.{ObservationPoint, ObservationTimeOfDay, Day}
import starling.daterange.ObservationPoint._
import starling.utils.ImplicitConversions._
import starling.db.{MarketDataReader, MarketDataStore}
import starling.marketdata._
import starling.pivot.{PivotEdits, Field}

class WithEditsMarketDataReader(reader:MarketDataReader, marketDataType:MarketDataType, allEdits:PivotEdits) extends MarketDataReader {

  /*val observationDayField = Field("Observation Day")
  val observationTimeField = Field("Observation Time")

  val groupedEdits: Map[(ObservationPoint, MarketDataKey), List[PivotEdit]] = allEdits.groupBy(edit => {
    val observationPoint = ObservationPoint(
      edit.value[Day](observationDayField),
      ObservationTimeOfDay.fromName(edit.value[String](observationTimeField))
    )

    val key: MarketDataKey = marketDataType.createKey(edit.keysAndMaybeValue)
    (observationPoint, key)
  }).mapValues(_.toList)

  val newMarketData:List[(TimedMarketDataKey, MarketData)] = groupedEdits.flatMap{ case ((point,key),edits) => {
    reader.read(key.dataType, Some(Set(point.day)), Some(Set(point.timeOfDay)), Some(Set(key))).headOption match {
      case Some(_) => Nil
      case None => List( (TimedMarketDataKey(point,key), marketDataType.createValue(edits.toList.map(_.keysAndMaybeValue))) )
  } } }.toList

  val allDeletes = allEdits.toList.filterCast[DeletePivotEdit].map { case delete => {
    val observationPoint = ObservationPoint(
      delete.value[Day](observationDayField),
      ObservationTimeOfDay.fromName(delete.value[String](observationTimeField))
    )
    val key: MarketDataKey = marketDataType.createKey(delete.keysAndMaybeValue)
    TimedMarketDataKey(observationPoint, key)
  }}*/

  def identifier = reader.identifier + " with edits " + allEdits

  def marketDataTypes = reader.marketDataTypes

  def read(marketDataType:MarketDataType,
           observationDays:Option[Set[Option[Day]]],
           observationTimes:Option[Set[ObservationTimeOfDay]],
           keys:Option[Set[MarketDataKey]]): List[(TimedMarketDataKey, MarketData)] = {
    assert(marketDataType == WithEditsMarketDataReader.this.marketDataType, "The market data types have to be the same: " +
            (marketDataType, WithEditsMarketDataReader.this.marketDataType))

    /*val read:List[(TimedMarketDataKey, MarketData)] = reader.read(marketDataType, observationDays, observationTimes, keys)

    val readWithEditsApplied = read.map {
      case (TimedMarketDataKey(point, key), marketData) => {
        groupedEdits.get((point, key)) match {
          case Some(edits) => {
            val currentRows = key.castRows(marketData)
            // Remove deletes from current rows.
            val deletes: List[DeletePivotEdit] = edits.filterCast[DeletePivotEdit]
            val currentRowsWithDeletesRemoved = currentRows.filterNot(currentMap => {
              deletes.exists(del => del.keysMatch(currentMap, observationDayField, observationTimeField))
            })

            // Add amendments
            val amendments = edits.filterCast[AmendPivotEdit]
            val newRows = amendments.toList.map(_.keysAndMaybeValue)
            val mixedRows = MarketDataStore.applyOverrideRule(marketDataType, currentRowsWithDeletesRemoved.toList ::: newRows.toList).toList

            (TimedMarketDataKey(point, key), marketDataType.createValue(mixedRows))
          }
          case _ => (TimedMarketDataKey(point, key), marketData)
        }
      }
    }

    val matchingNewMarketData = newMarketData.filter {
      case (TimedMarketDataKey(point, key),marketData) => {
        observationDays.map(_.contains(point.day)).getOrElse(true) &&
                observationTimes.map(_.contains(point.timeOfDay)).getOrElse(true) &&
                keys.map(_.contains(key)).getOrElse(true)
      }
    }

    readWithEditsApplied ::: matchingNewMarketData*/
    throw new Exception("bla")
  }

  def readAllObservationDayAndMarketDataKeys(marketDataType:MarketDataType):List[TimedMarketDataKey] = {
    /*assert(marketDataType == WithEditsMarketDataReader.this.marketDataType, "The market data types have to be the same: " +
            (marketDataType, WithEditsMarketDataReader.this.marketDataType))

    val originalTimedKeys = reader.readAllObservationDayAndMarketDataKeys(marketDataType)
    (originalTimedKeys.toSet -- allDeletes.toSet).toList ::: newMarketData.map(_._1)*/
    throw new Exception("bla")
  }

}