package starling.eai.instrumentreaders

import starling.systemofrecord.{InstrumentReader, SystemOfRecord}
import starling.richdb.{RichDB, RichResultSetRow}
import starling.utils.sql.QueryBuilder._
import starling.db.EAITradeSystem
import starling.tradestore.eai.{EAITradeAttributes}
import starling.eai.TreeID
import starling.daterange.{Day, Timestamp}
import starling.daterange.Day._
import starling.instrument._
import starling.quantity.Quantity
import starling.quantity.UOM._
import starling.trade.{TradeID, Trade}
import starling.market._
import starling.utils.{Log, Reflection}
import starling.utils.sql.Clause
import starling.utils.sql.Query

/**
 * version is an option on a map of bookid to downloadid
 */
case class EAISystemOfRecord(externalDB: RichDB, bookID: Int, downloadID: Int) extends SystemOfRecord(externalDB) {
  import EAISystemOfRecord._
  private val etOptions = ((select ("tradeType =  "+ET_OPTION+", *") from ("tblPlutoETOptions t")), ("ContractDate" gt (31 Sep 2009)))
  private val futures = ((select ("tradeType =  "+FUTURE+",*") from ("tblPlutoFutures t")), ("ContractDate" gt (31 Sep 2009)))
  private val swaps = ((select("tradeType =  "+SWAP+",*") from ("tblPlutoSwaps t")), (("endDate" gt (31 Sep 2009)) and ("FloatingFloating" eql 0)))
  private val swapSpreads = ((select("tradeType =  "+SWAP_SPREAD+",*") from ("tblPlutoSwaps t")), (("endDate" gt (31 Sep 2009)) and ("FloatingFloating" eql 1)))
  private val clearportSwaps = ((select("tradeType =  " + CLEARPORT_SWAP + ",*") from ("tblPlutoClearPortSwaps t")), ("endDate" gt (31 Sep 2009)))
  private val otcOptions = ((select("tradeType =  " + OTC_OPTION + ",*") from ("tblPlutoOTCOptions t")), ("expiryDate" gt (31 Sep 2009)))
  private val cfds = ((select("tradeType =  " + CFD_SWAP + ",*") from ("tblPlutoCFDTrades t")), ("1" eql "1"))
  private val cargos = ((select("tradeType =  " + CARGO + ",*") from ("tblPlutoCargos t")), ("1" eql "1"))

  private def queries = generateQueries(List(swaps, clearportSwaps, otcOptions, etOptions, futures, cfds, swapSpreads/*, cargos*/))

  private def generateQueries(list: List[(Query, Clause)]) = {
    list.map {
      case (q, cl) =>
        (q
          leftJoin ("EAI.dbo.vwCostTypes cts", ("cts.ID" eql "t.CommissionCostTypeID"))
          where (("t.downloadid" eql downloadID) and ("bookid" eql bookID) and ("isdeleted" eql 0) and ("deletedinaspect" eql 0) and cl)
          )
    }
  }

  protected val readers: List[InstrumentReader] = {
    new Reflection().listClassesOfType("starling.eai.instrumentreaders", classOf[InstrumentReader]).map(c => c.newInstance) :::
    new Reflection().listClassesOfType("starling.eai.instrumentreaders.physical", classOf[InstrumentReader]).map(c => c.newInstance)
  }

  def allTrades(f: (Trade) => Unit): (Int, Set[String]) = {
    queries.map(allTrades(_)(f)).reduceLeft((b, a) => (a._1 + b._1, a._2 ++ b._2))
  }

  def trade(tradeID: String)(f: (Trade) => Unit) = {
    val queries = generateQueries(tradeID.head.toUpper match {
      case 'O' => List(etOptions, otcOptions)
      case 'F' => List(futures)
      case 'S' => List(swaps, swapSpreads, cfds)
      case 'C' => List(clearportSwaps)
      case 'P' => List(cargos)
    })
    var found = List[Trade]()
    queries.foreach(query => allTrades(query and ("aspectid" eql tradeID))(t => found ::= t))
    found match {
      case t :: Nil => f(t)
      case Nil =>
      case _ => throw new Exception("Found multiple matches: " + found)
    }
  }

  protected def createNullTrade(rs: RichResultSetRow) = {
    val tradeID = TradeID(rs.getString("aspectid"), EAITradeSystem)
    val tradeDay = rs.getDay("TradeDate")
    val counterParty = if(rs.hasColumn("counterparty")) {
      rs.getString("counterparty")
    } else {
      "No CounterParty"
    }

    val trader = rs.getString("trader") // Not available yet in EAI Archive, usually the same as tradedFor
    val tradedFor = rs.getString("trader") // who this trade was traded for
    val broker = rs.getString("broker")
    val timestamp = Timestamp.parse("01Jan1970 12:21:12.999")// Not available in EAI Archive

    val strategyID = TreeID(rs.getInt("StrategyID"))
    val bookID = TreeID(rs.getInt("BookID"))
    val dealID = TreeID(rs.getInt("DealID"))
    val clearingHouse = rs.getInt("TradeType") match {
      case CLEARPORT_SWAP => "ClearPort"
      case _ => ""
    }

    val attributes = EAITradeAttributes(strategyID, bookID, dealID, trader, tradedFor, broker, clearingHouse)

    Trade(tradeID, tradeDay, counterParty, attributes, NullInstrument())
  }

  override protected def addCostsToTrade(trade: Trade, rs: RichResultSetRow) = {
    var costs: List[Costs] = Nil

    val settlementDay = trade.tradeDay // it could be trade day + 1 business day but this way gives us no miniscule theta
    val valuationCCY = USD // assuming USD, aspect always has USD and the DB doesn't specify a currency.

    if (rs.hasColumn("premium")) {
      val volume = trade.asUtpPortfolio.portfolio.map {
        case (utp: Instrument, volume) => utp.volume * volume
      }.sum
      val premium = Quantity(rs.getDouble("premium"), valuationCCY / volume.uom)
      val premiumCosts = PremiumCosts(settlementDay, trade.counterParty, volume, premium)
      costs ::= premiumCosts
    }

    if (rs.hasColumn("Commission")) {
      val name = rs.getString("name")
      val code = rs.getString("CommissionCostType")
      val commission = Quantity(rs.getDouble("commission"), valuationCCY)
      if (!commission.isZero) {
        val commissionCosts = CommissionCosts(name, code, settlementDay, trade.counterParty, commission)
        costs ::= commissionCosts
      }
    }

    trade.copy(costs = costs)
  }

  override protected def createInstrument(rs: RichResultSetRow) = {
    if(rs.isNull("ID")) {
      ErrorInstrument("Trade has a NULL ID so is not assigned in Aspect. Trade details: " + rs)
    } else {
      super.createInstrument(rs)
    }
  }
}

object EAISystemOfRecord {
  val LONDON_DERIVS_BOOK = 43
  val GasolineSpec = 149
  val LONDON_DERIVS_OPTIONS_BOOK = 173
  val CrudeSpecNorthSea = 197
  val HoustonDerivatives = 190
  val BOOKS = List(LONDON_DERIVS_BOOK, LONDON_DERIVS_OPTIONS_BOOK, CrudeSpecNorthSea, GasolineSpec, HoustonDerivatives)

  val ET_OPTION = 1
  val FUTURE = 2
  val SWAP = 3
  val OTC_OPTION = 4
  val CLEARPORT_SWAP = 5
  val CFD_SWAP = 6
  val SWAP_SPREAD = 7
  val CARGO = 8

  val CALENDAR_SPREAD_TYPE = "Calendar Spread"
  val ASIAN_TYPE = "Asian"
  val AVERAGE_TYPE = "Average"

}
