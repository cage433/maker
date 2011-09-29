package starling.tradestore

import com.google.common.collect.MapMaker
import collection.immutable.TreeMap
import collection.{SortedMap, MapProxy}
import starling.pivot.{FieldDetails, Field}
import starling.daterange.{Day, Timestamp}
import starling.instrument.{TradeID, Trade}
import collection.mutable.{HashMap, Map => MMap, Set => MSet}
import starling.utils.cache.CacheFactory
import starling.utils.AppendingMap
import starling.instrument.{ErrorInstrument, Tradeable, UTP}

class TradeHistory

case class SingleTradeIDTradeVersions( tradeID : TradeID, 
                                       earliest : Timestamp,
                                       latest : Timestamp,
                                       versions : TreeMap[Timestamp, TradeAndFields]
)
{

  def this(tradeID : TradeID, ts : Timestamp, value : TradeAndFields) = {
    this(tradeID, ts, ts, TreeMap[Timestamp, TradeAndFields]() + (ts -> value))
  }

  def +(ts : Timestamp, value : TradeAndFields) : SingleTradeIDTradeVersions = {
    if (versions.contains(ts)){
      assert(versions(ts) == value, "Two versions of a trade (with the same trade id) with the same timestamp: new " + (ts,value) + ", original " + versions(ts))
      this
    } else {
      new SingleTradeIDTradeVersions(tradeID, earliest min ts, latest max ts, versions + (ts -> value))
    }
  }

  def version(timestamp : Option[Timestamp]) : Option[TradeAndFields] = {
    versionWithTimestamp(timestamp.getOrElse(latest)) match {
      case Some((_, v)) => Some(v)
      case None => None
    }
  }
  def versionWithTimestamp(timestamp : Timestamp) : Option[(Timestamp, TradeAndFields)] = {
    val before = versions.rangeImpl(None, Some(timestamp.next))
    if (before.isEmpty)
      None
    else {
      val timestampTradeAndFieldsPair = before.last
      if (timestampTradeAndFieldsPair._2.trade.isDeletedTrade) None else Some(timestampTradeAndFieldsPair)
    }
  }
  def version(timestamp : Timestamp) : Option[TradeAndFields] = version(Some(timestamp))
  def latestVersion:Option[TradeAndFields] = version(Some(latest))
}

class MultipleTradeIDTradeVersions extends MapProxy[TradeID, SingleTradeIDTradeVersions]{
  val self = MMap[TradeID, SingleTradeIDTradeVersions]()

  def += (tradeID : TradeID, timestamp : Timestamp, value : TradeAndFields){
    if (contains(tradeID)){
      self += (tradeID -> (self(tradeID) + (timestamp, value)))
    } else {
      self += tradeID -> new SingleTradeIDTradeVersions(tradeID, timestamp, value)
    }
  }
}

case class TradeAndFields(id:Int, trade:Trade, fields:AppendingMap[Field, Any]=new AppendingMap(new TreeMap())) {
  def matches(tradeFilter:FieldDetailsTradeSelection) = tradeFilter(fields)
  def withNewInstrument(tradeable:Tradeable) = {
    val maps = fields.namedMaps.updated("Instrument", TradeableFields.createFieldValues(trade, tradeable))
    TradeAndFields(id, trade.copy(tradeable=tradeable), new AppendingMap(maps))
  }
}

class TradeHistories() {

  val versionedTrades = new MultipleTradeIDTradeVersions
  val utpDetails = MMap[UTP, Map[Field, Any]]()
  val utps = MMap[UTP, Int]()
  val tradeUTPs = MMap[Trade, Map[UTP, Double]]()

  val lock = new Object()

  val rowsCache = CacheFactory.getCache("TradeHistories.tradeRowsAsOf")

  var latestTimestamp : Timestamp = Timestamp(0)

  def addTrade(id : Int, timestamp : Timestamp, trade : Trade, fieldValues : AppendingMap[Field,Any]){

    def getUtpID(utp: UTP) = {
      utps.get(utp) match {
        case Some(id) => id
        case None => {
          val id = utps.size
          utps += (utp -> id)
          id
        }
      }
    }

    lock.synchronized{

      latestTimestamp = latestTimestamp max timestamp
      versionedTrades += (trade.tradeID, timestamp, TradeAndFields(id, trade, fieldValues))

      val utpPortfolio: Map[UTP, Double] = if (!trade.isDeletedTrade) trade.asUtpPortfolio.portfolio else Map()
      tradeUTPs += trade -> utpPortfolio
      utpPortfolio.keys.foreach {
        utp =>
          if (! utpDetails.contains(utp)){
            utps += (utp -> getUtpID(utp))
            //utpDetails += utp -> ToFieldMap(utp.details)
          }
      }
    }
  }

  def latestTradeRows = lock.synchronized {
    Map[TradeID, TradeRow]() ++ versionedTrades.flatMap {
      case (tradeID, history) => {
        history.latestVersion match {
          case Some(TradeAndFields(id, trade, _)) => {
            Some(tradeID -> TradeRow(id, history.latest, trade))
          }
          case None => None
        }
      }
    }
  }

  def tradesAsOf(timestamp : Timestamp, expiryDay:Day, tradePredicate : FieldDetailsTradeSelection) = {
    tradeRowsAsOf(timestamp, Some(expiryDay), tradePredicate).map(_.trade)
  }

  def tradeRowsAsOf(timestamp : Timestamp, expiryDay:Option[Day], tradePredicate : FieldDetailsTradeSelection = FieldDetailsTradeSelection.Null, marketDay:Option[Day]=None):List[TradeAndFields] = {
    lock.synchronized {
      rowsCache.memoize( (timestamp, expiryDay, tradePredicate, marketDay), {
        val rows = new scala.collection.mutable.ArrayBuffer[TradeAndFields]()
        versionedTrades.foreach {
          case (_, history) => history.version(timestamp) match {
            case None =>
            case Some(tradeAndFields) => {
              val expiryMatches = (expiryDay, tradeAndFields.trade.expiryDay) match {
                case (None, _) => true
                case (_, None) => true
                case (Some(f), Some(t)) => t >= f
              }
              if (expiryMatches && marketDay.map(_ >= tradeAndFields.trade.tradeDay).getOrElse(true) && tradePredicate(tradeAndFields.fields)) {
                rows += tradeAndFields
              }
            }
          }
        }
        rows.toList
      })
    }
  }
}