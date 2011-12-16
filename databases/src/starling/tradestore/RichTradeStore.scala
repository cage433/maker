package starling.tradestore


import starling.pivot.{Field => PField}
import starling.pivot.Field._
import starling.pivot._
import controller.PivotTableConverter
import model.{PivotTableModel, UndefinedValue}
import starling.utils.ImplicitConversions._
import starling.instrument._
import starling.pivot._
import java.lang.String
import collection.Seq
import starling.daterange._
import starling.instrument.{Trade, TradeSystem}
import starling.quantity._
import starling.gui.api._
import starling.marketdata.PeriodFieldDetails
import starling.tradeimport.ClosedDesks
import java.util.concurrent.atomic.AtomicReference
import starling.richdb.{RichInstrumentResultSetRow, RichDB}
import starling.dbx.QueryBuilder._
import starling.dbx.LiteralString._
import starling.utils.{STable, Log}
import starling.utils.cache.CacheFactory
import collection.immutable.{TreeMap, Map, Set, List}
import scalaz.Scalaz._
import collection.parallel.immutable.ParMap

//This is the code which maps from TradeableType.fields to FieldDetails
object TradeableFields {

  private val fieldDetailsAndMapper: Seq[(FieldDetails, (Trade, Any) => Any)] = TradeableType.fieldsWithType.map {
    case (name, klass) => {
      val dateRange = classOf[DateRange]
      val quantity = classOf[Quantity]
      val spreadOrQuantity = classOf[SpreadOrQuantity]
      val period = classOf[Period]
      (name, klass) match {
        case ("Quantity", _) => (new TradeIDGroupingSumPivotQuantityFieldDetails("Quantity"), (t: Trade, q: Any) => Map(t.tradeID.toString -> q))
        case (_, `dateRange`) => (new PeriodFieldDetails(name), (t: Trade, v: Any) => {
          v match {
            case d: DateRange => DateRangePeriod(d)
            case s: Spread[_] => SpreadPeriod(s.first, s.last)
            case s: Strip[_] => StripPeriod(s.first, s.last)
            case p: Period => p
            case _ => throw new Exception(v + " is not a DateRange or Period")
          }
        })
        case (_, `spreadOrQuantity`) => (new PivotSpreadQuantityFieldDetails(name), (t: Trade, v: Any) => {
          v match {
            case q: Quantity => SpreadOrQuantity(Left(q))
            case sp: SpreadQuantity => SpreadOrQuantity(Right(sp))
            case s: SpreadOrQuantity => s
          }
        })
        case (_, `period`) => (new PeriodFieldDetails(name), (t: Trade, p: Any) => p)
        case (_, `quantity`) => (new QuantityLabelFieldDetails(name), (t: Trade, q: Any) => q)
        case (_, _) => (FieldDetails(name), (t: Trade, v: Any) => v.toString.asInstanceOf[Any])
      }
    }
  }
  val keysCache = CacheFactory.getCache("TradeableFields.keysCache", unique = true)

  private val normalizedNameToFieldAndMapper: Map[String, (FieldDetails, (Trade, Any) => Any)] = Map() ++ fieldDetailsAndMapper.map {
    t => keysCache.memoize(t._1, t._1.field.name.removeWhiteSpace.toLowerCase) -> t
  }

  val fieldDetails = fieldDetailsAndMapper.map(_._1).toList

  def createFieldValues(trade: Trade, tradeable: Tradeable): Map[PField, Any] = {
    val tradeableDetails: Map[String, Any] = tradeable.shownTradeableDetails
    tradeableDetails.map {
      case (k, v) => {
        val (fieldDetails, mapper) = keysCache.memoize(k, normalizedNameToFieldAndMapper(k.toLowerCase.removeWhiteSpace))
        (fieldDetails.field, mapper(trade, v))
      }
    }
  }
}

case class TradeAndFields(id: Int, trade: Trade, fields: Map[Field, Any] = new TreeMap()) {
  assert(trade != null)
  assert(fields != null)

  def matches(tradeFilter: FieldDetailsTradeSelection) = tradeFilter(fields)

  def withNewInstrument(tradeable: Tradeable) = {
    val maps = fields ++ TradeableFields.createFieldValues(trade, tradeable)
    TradeAndFields(id, trade.copy(tradeable = tradeable), maps)
  }
}

object TradeAndFields {
  def apply(tradeRow: TradeRow): TradeAndFields = {
    new TradeAndFields(tradeRow.id, tradeRow.trade, createFieldValues(tradeRow.trade))
  }

  private def createFieldValues(trade: Trade): Map[Field, Any] = {
    val justTradeDetails: Map[Field, Object] = JustTradeFields.createFieldValues(trade) // 2s
    val tradeAttributeFields: Map[Field, Any] = trade.attributes.createFieldValues // 4s
    val instrumentDetails: Map[Field, Any] = TradeableFields.createFieldValues(trade, trade.tradeable) // 4s
    justTradeDetails ++ tradeAttributeFields ++ instrumentDetails
  }
}

//This is the code which defines and creates the Trade Fields (not the Tradeable or TradeAttribute fields)
object JustTradeFields {
  val tradeCountField = PField("Trade Count")
  val tradeIDField = PField("Trade ID")
  val tradeDayField = PField("Trade Day")
  val counterpartyField = PField("Counterparty")
  val instrumentField = PField("Instrument")
  val initialPriceField = PField("Initial Price")

  def createFieldValues(trade: Trade) = {
    val idAsLabel = TradeIDLabel(trade.tradeID.id, TradeSystemLabel(trade.tradeID.tradeSystem.name, trade.tradeID.tradeSystem.shortCode))
    Map(
      tradeCountField -> idAsLabel,
      tradeIDField -> idAsLabel,
      tradeDayField -> trade.tradeDay,
      counterpartyField -> trade.counterParty,
      instrumentField -> trade.tradeable.tradeableType.name
    ) ++ (trade.premium match {
      case Some(p) => Map(initialPriceField -> SpreadOrQuantity(Left(p)))
      case None => Map()
    })
  }

  val fieldDetails = List(
    new FieldDetails("Trade Count") {
      override def isDataField = true

      override def transformValueForGroupByField(a: Any) = "Undefined"

      override def value(a: Any) = a.asInstanceOf[Set[_]].size
    },
    new FieldDetails("Trade ID") {
      override def formatter = TradeIDPivotFormatter
    },
    FieldDetails("Trade Day"),
    FieldDetails("Counterparty"),
    FieldDetails("Instrument"),
    new QuantityLabelFieldDetails("Initial Price")
  )
  val fields = fieldDetails.map(_.field)
}

case class FieldDetailsTradeSelection(filter: List[(FieldDetails, Selection)], selection: List[List[(FieldDetails, Selection)]]) {
  def apply(data: Map[PField, Any]) = {
    def matches(fs: (FieldDetails, Selection)): Boolean = {
      try {
        val (fieldDetail, selection) = fs
        val value = data.getOrElse(fieldDetail.field, UndefinedValue)
        selection.matches(fieldDetail, value)
      } catch {
        case e => {
          println("data is ")
          data.foreach(println)
          throw e
        }
      }
    }
    filter.forall(matches) && (selection.isEmpty || selection.exists(_.forall(matches)))
  }
}

object FieldDetailsTradeSelection {
  val Null = FieldDetailsTradeSelection(List(), List())
}

case class TradeChanges(
                         fields: List[FieldDetailsGroup] = List(),
                         movedIn: List[TradeAndFields] = List(),
                         movedOut: List[TradeAndFields] = List(),
                         created: List[TradeAndFields] = List(),
                         deleted: List[TradeAndFields] = List(),
                         undeleted: List[TradeAndFields] = List(),
                         amended: List[(TradeAndFields, TradeAndFields)] = List()) {
  def removeUntraded(marketDay: Day) = {
    //Maybe Trade should have no mtm before its trade day but that would be disruptive and requires some thought
    def filter(trades: List[TradeAndFields]) = trades.filter(r => r.trade.tradeDay <= marketDay)
    //There is still an assue with amendments to the trade day. For consistency this should be fixed in Trade
    TradeChanges(fields, filter(movedIn), filter(movedOut), filter(created), filter(deleted), filter(undeleted), amended)
  }

  def changes = movedIn.size + movedOut.size + created.size + deleted.size + undeleted.size + amended.size
}

case class SingleTradeIDTradeVersions(tradeID: TradeID,
                                      earliest: Timestamp,
                                      latest: Timestamp,
                                      versions: TreeMap[Timestamp, TradeAndFields]
                                       )

object SingleTradeIDTradeVersions {
  def apply(tradeID: TradeID, versions: TreeMap[Timestamp, TradeRow]): SingleTradeIDTradeVersions = {
    assert(versions.nonEmpty, "No versions for trade? " + tradeID)
    val earliest = versions.head._1
    val latest = versions.last._1
    val v = versions.map {
      case (k, v) => (k -> TradeAndFields(v))
    }
    new SingleTradeIDTradeVersions(tradeID, earliest, latest, v)
  }
}

abstract class RichTradeStore(db: RichDB, tradeSystem: TradeSystem, closedDesks: ClosedDesks) extends TradeStore(db, tradeSystem, closedDesks) {

  import concurrent.stm._

  case class UTPData(utp: UTP, id: Int, volume: Double)

  val tradeUTPsMap = TMap[Trade, Iterable[UTPData]]()
  val utpIDMap = TMap[UTP, Int]()

  private def utpsForTrade(trade: Trade) = atomic {
    implicit txn => {
      tradeUTPsMap.get(trade) match {
        case None => {
          val utpPortfolio: Map[UTP, Double] = if (!trade.isDeletedTrade) trade.asUtpPortfolio.portfolio else Map()
          val a = utpPortfolio.map {
            case (utp, volume) => utpIDMap.get(utp) match {
              case Some(id) => UTPData(utp, id, volume)
              case None => {
                val id = utpIDMap.size + 1
                utpIDMap += (utp -> id)
                UTPData(utp, id, volume)
              }
            }
          }
          tradeUTPsMap += trade -> a
          a
        }
        case Some(u) => u
      }
    }
  }

  def tradeHistory(tradeID: TradeID): Option[(STable, List[FieldDetailsGroup], List[CostsLabel])] = {
    val historyForTrade = singTradeHistory(tradeID)
    if (historyForTrade.nonEmpty) {
      val history = SingleTradeIDTradeVersions(tradeID, historyForTrade)
      val allTradeableTypes = history.versions.map {
        case (_, tradeAndFields) => tradeAndFields.trade.tradeable.tradeableType
      }.toSet

      val costsVersions = history.versions.map {
        case (_, tradeAndFields) => {
          val costs = tradeAndFields.trade.costs
          CostsLabel(costs.map(cost => {
            val value = cost match {
              case commission: CommissionCosts => commission.commission
              case premium: PremiumCosts => premium.premium
              case ordinary: OrdinaryCost => ordinary.quantity
              case _ => Quantity.NULL
            }
            CostsInfo(cost.costType, value, cost.info)
          }))
        }
      }
      val mapList: List[Map[PField, Any]] = (for (((timestamp, tradeAndFields), version) <- history.versions.toMap.zipWithIndex) yield {
        Map(PField("Version") -> (version + 1), PField("Import Time") -> timestamp) ++ tradeAndFields.fields ++ joiningTradeAttributeFieldValues(tradeAndFields.trade.attributes)
      }).toList
      val fieldDetailsGroups0 = createFieldDetailGroups(allTradeableTypes)
      val ds = new UnfilteredPivotTableDataSource() {
        val fieldDetailsGroups = {
          FieldDetailsGroup("Magic", FieldDetails("Version"), FieldDetails("Import Time")) :: fieldDetailsGroups0
        }

        def unfilteredData(pfs: PivotFieldsState) = mapList
      }
      val measures = ds.fieldDetails.filter(fd => (fd.field.name != "Trade Count" && fd.field.name != "Version"))
      val pivotTable = PivotTableModel.createPivotTableData(ds, PivotFieldsState(rowFields = List(PField("Version")), dataFields = measures.map(_.field)))
      Some(new PivotTableConverter(OtherLayoutInfo(), pivotTable).toSTable("Trade History"), fieldDetailsGroups0, costsVersions.toList)
    } else {
      None
    }
  }

  private def fromPredicate(tradePredicate: TradePredicate) = {
    def mapToFieldDetails(a: (PField, Selection)) = allPossibleFieldDetails(a._1) -> a._2
    FieldDetailsTradeSelection(tradePredicate.filter.map(mapToFieldDetails), tradePredicate.selection.map(_.map(mapToFieldDetails)))
  }

  def tradeChanges(t1: Timestamp, t2: Timestamp, expiryDay: Day, tradePredicate: TradePredicate): TradeChanges = {
    def rowsFor(t: Timestamp) = {
      val trades: List[TradeAndFields] = tradeAndFields(t, Some(expiryDay), None)
      trades.map {
        row => {
          //Add join based fields like Strategy and 'Group Company'
          val fields = row.fields ++ joiningTradeAttributeFieldValues(row.trade.attributes)
          val newRow = TradeAndFields(row.id, row.trade, fields)
          (row.trade.tradeID, newRow)
        }
      }.toMap
    }

    var t1Trades = rowsFor(t1)
    var t2Trades = rowsFor(t2)

    val tradeSelection = fromPredicate(tradePredicate)
    val tradeIds = (t1Trades.keySet ++ t2Trades.keySet).filter {
      tradeID =>
        List(t1Trades.get(tradeID), t2Trades.get(tradeID)).flatten.exists(r => tradeSelection(r.fields))
    }
    t1Trades = t1Trades.filterKeys(tradeIds)
    t2Trades = t2Trades.filterKeys(tradeIds)

    val groupedt2Trades = t2Trades.groupBy {
      case (t2TradeId, t2TradeRow) =>
        t1Trades.get(t2TradeId) match {
          case None => "Created"
          case Some(t1TradeRow) => {
            if (t1TradeRow.trade == t2TradeRow.trade)
              "Unchanged"
            else if (t1TradeRow.trade.isDeletedTrade && !t2TradeRow.trade.isDeletedTrade)
              "Undeleted"
            else if (!t1TradeRow.trade.isDeletedTrade && t2TradeRow.trade.isDeletedTrade)
              "Deleted"
            else if (t1TradeRow.matches(tradeSelection) && !t2TradeRow.matches(tradeSelection))
              "Moved Out"
            else if (!t1TradeRow.matches(tradeSelection) && t2TradeRow.matches(tradeSelection))
              "Moved In"
            else
              "Amended"
          }
        }
    }
    def valuesList(map: scala.collection.Map[TradeID, TradeAndFields]) = map.valuesIterator.toList

    val allTradeableTypes = Set() ++ t1Trades.values.map(_.trade.tradeable.tradeableType) ++ t2Trades.values.map(_.trade.tradeable.tradeableType)

    val deletedTrades = t1Trades.filterKeys(id => !t2Trades.contains(id))
    TradeChanges(
      createFieldDetailGroups(allTradeableTypes),
      valuesList(groupedt2Trades.getOrElse("Moved In", Map())),
      valuesList(groupedt2Trades.getOrElse("Moved Out", Map())),
      valuesList(groupedt2Trades.getOrElse("Created", Map())),
      valuesList(deletedTrades),
      valuesList(groupedt2Trades.getOrElse("Undeleted", Map())),
      groupedt2Trades.getOrElse("Amended", Map()).map {
        case (tradeID, tradeT2) => (t1Trades(tradeID), tradeT2)
      }.toList
    )
  }

  private def tradeAndFields(timestamp: Timestamp, expiryDay: Option[Day], marketDay: Option[Day]) = {
    val all = readAll(timestamp, expiryDay, marketDay = marketDay)
    val trades: List[TradeAndFields] = all.values.map(TradeAndFields.apply(_)).toList
    trades
  }

  def readAll(timestamp: Timestamp, tradePredicate: TradePredicate, expiryDay: Option[Day], marketDay: Option[Day]): (List[FieldDetailsGroup], List[TradeAndFields]) = {
    val trades: List[TradeAndFields] = tradeAndFields(timestamp, expiryDay, marketDay)
    val filter = fromPredicate(tradePredicate)
    var allTradeableTypes = Set[TradeableType[_]]()
    val rows = trades.flatMap {
      tradeAndFields =>
        val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(tradeAndFields.trade.attributes)
        val tradeFields = joinedTradeAttributeDetails ++ tradeAndFields.fields
        if (filter(tradeFields)) {
          allTradeableTypes += tradeAndFields.trade.tradeable.tradeableType
          Some(TradeAndFields(tradeAndFields.id, tradeAndFields.trade, tradeFields))
        } else {
          None
        }
    }
    val fieldDetails = createFieldDetailGroups(allTradeableTypes.toSet)
    (fieldDetails, rows)
  }

  def reportPivot(
                   timestamp: Timestamp,
                   marketDay: Day,
                   expiryDay: Day,
                   predicate: TradePredicate,
                   utpPartitioningFields: List[PField],
                   addRows: Boolean //Before the user has pressed run we just show the fields
                   ): PivotTableDataSource = pivot(timestamp, Some(marketDay), expiryDay, predicate, utpPartitioningFields, addRows)


  def pivot(
             timestamp: Timestamp,
             marketDay: Option[Day],
             expiryDay: Day,
             predicate: TradePredicate
             ): PivotTableDataSource = pivot(timestamp, marketDay, expiryDay, predicate, Nil, true)

  private val pivotCache = CacheFactory.getCache("TradeStore.pivot")

  def pivot(
             timestamp: Timestamp,
             marketDay: Option[Day],
             expiryDay: Day,
             predicate: TradePredicate,
             utpPartitioningFields: List[PField],
             addRows: Boolean): PivotTableDataSource = {
    pivotCache.memoize(
    (timestamp, marketDay, expiryDay, predicate, utpPartitioningFields, addRows), {
      createPivot(timestamp, marketDay, expiryDay, predicate, utpPartitioningFields, addRows)
    }
    )
  }

  /**
   * Add fields whose value we don't know at the time of trade field construction. E.g. the country name
   * corresponding to a titan country code
   */
  protected def addExtraInstrumentFields(map: Map[PField, Any]) = map

  protected def addExtraInstrumentFieldDetails(list: List[FieldDetails]) = list

  private def createPivot(
                           timestamp: Timestamp,
                           marketDay: Option[Day],
                           expiryDay: Day,
                           predicate: TradePredicate,
                           utpPartitioningFields: List[PField],
                           addRows: Boolean
                           ): PivotTableDataSource = {
    val filter = fromPredicate(predicate)
    new UnfilteredPivotTableDataSource() {
      private var tradeableTypes = Set[TradeableType[_]]()
      val pivotData = {
        Log.infoWithTime("Building pivot data") {
          val versions = readAll(timestamp, Some(expiryDay), marketDay = marketDay).values
          versions.par.flatMap {
            case version => {
              val (trade, details) = TradeAndFields(version) |> (v => (v.trade, v.fields))
              tradeableTypes += trade.tradeable.tradeableType

              val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(trade.attributes)

              val tradeFields: Map[Field, Any] = joinedTradeAttributeDetails ++ details
              if (filter(tradeFields)) {
                val utps = utpsForTrade(trade)
                utps.map {
                  case UTPData(utp, id, volume) => {
                    val utpID = {
                      val partitions: Map[String, String] = utpPartitioningFields.map {
                        field => (field.name, joinedTradeAttributeDetails(field).toString)
                      }.toMap
                      new UTPIdentifier(id, partitions)
                    }
                    Map(
                      PField(instrumentID_str) -> utpID,
                      PField(tradeCount_str) -> details(PField(tradeID_str)),
                      PField(utpVolume_str) -> volume
                    ) ++ tradeFields
                  }

                }
              } else {
                Nil
              }
              //              }
            }
          }.toList
        }
      }
      println("There are " + pivotData.size + " rows")

      def unfilteredData(pfs: PivotFieldsState) = {
        if (addRows) pivotData else List()
      }

      val fieldDetailsGroups: List[FieldDetailsGroup] = createFieldDetailGroups(tradeableTypes.toSet)
      override val drillDownGroups = pivotDrillDownGroups

      override def initialState = pivotInitialState(tradeableTypes.toSet)
    }
  }

  private def createFieldDetailGroups(tradeableTypes: Set[TradeableType[_]]) = {
    val allFieldNames = Set() ++ tradeableTypes.flatMap(_.fields)
    val instrumentFieldDetails: List[FieldDetails] = addExtraInstrumentFieldDetails(
      tradeableFieldDetails.filter(
        fd => allFieldNames.contains(fd.field.name)
      )
    )
    List(
      FieldDetailsGroup("Trade Fields", JustTradeFields.fieldDetails),
      FieldDetailsGroup("Trade System Fields", tradeAttributeFieldDetails),
      FieldDetailsGroup("Instrument Fields", instrumentFieldDetails)
    ).filter(_.fields.nonEmpty)
  }

  def utps(timestamp: Timestamp, marketDay: Day, expiryDay: Day, predicate: TradePredicate, utpPartitioningFields: List[PField]): Map[UTPIdentifier, UTP] = Log.infoWithTime("Creating UTPs") {
    val filter = fromPredicate(predicate)
    val versions = readAll(timestamp, Some(expiryDay), marketDay = Some(marketDay)).values
    val a = versions.par.flatMap {
      case version => {
        val (trade, details) = TradeAndFields(version) |> (v => (v.trade, v.fields))
        val joinedTradeAttributeDetails = joiningTradeAttributeFieldValues(trade.attributes)
        val tradeFields = joinedTradeAttributeDetails ++ details
        if (filter(tradeFields)) {
          val utps = utpsForTrade(trade)
          utps.map {
            case UTPData(utp, id, volume) => {
              val utpID = {
                val partitions = utpPartitioningFields.map {
                  field => (field.name, tradeFields(field).toString)
                }.toMap
                new UTPIdentifier(id, partitions)
              }
              (utpID -> utp)
            }
          }
        } else {
          Nil
        }
      }
    }
    Map[UTPIdentifier, UTP]() ++ a
  }

  def pivotInitialState(tradeableTypes: Set[TradeableType[_]]): DefaultPivotState

  protected val instrumentFilteredDrillDown = {
    import starling.pivot.{
    Field => PivotField
    }
    val fallBack = PivotAxis(TradeableType.drillDownFields.map(f => PivotField(f)), List(PivotField("Trade ID")), List(), false)
    val instrumentToFields = TradeableType.types.flatMap(inType => {
      inType match {
        case y: TradeableType[_] => {
          Some((y.name, y.fields.map(PivotField(_))))
        }
        case n => None
      }
    })
    DrillDownInfo(fallBack, Some(FilteredDrillDown(PivotField("Instrument"), instrumentToFields)))
  }

  def pivotDrillDownGroups(): List[DrillDownInfo]

}