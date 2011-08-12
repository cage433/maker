package starling.neptune

import instrumentreaders.{RefinedFixationReader}
import starling.richdb.{RichResultSetRow, RichDB}
import java.lang.String
import starling.db.{RefinedFixationTradeSystem}
import starling.instrument._
import starling.utils.{StackTraceToString, Log}
import starling.trade.{Trade, TradeID}
import starling.systemofrecord.{SystemOfRecordBackedByADatabase, InstrumentReader}

class RefinedFixationSystemOfRecord(externalDB : RichDB) extends SystemOfRecordBackedByADatabase(externalDB){

  /**
   * For the pivot table, we need to group by
   *
   * BV           - SPD.COMPANY_CODE
   *    COMMODITY ME_SP_TRANSFER    - COMMODITY + EXCHANGE - i.e. the futures market used to price this
   *    COMMODITY                   - Lookup from MATERIALCODE
   *    EXCHANGE                    - S.MARKET
   *    HUB                         - Gl.DESCRIPTION
   *    RISK AREA                   - COU.NAME
   *    ALLOCATION STATUS           - ALLOCATED/UNALLOCATED depending on whether
   *                                    SAD.ALLOCATION_NO is null or not
   *    COUNTERPARTY                - CLI.NAME
   *    GROUP1                      - "RISK CARD" hard coded in spreadsheet
   *    GROUP                       - Either Physical, Unpriced or Futures.
   *    HUB1                        - Same as HUB
   *    RISK AREA 1                 - Same as 'RISK AREA'
   *    PRICING TYPE                - Blank for physicals
   *    COMMODITY CATEGORY          - C.DESCRIPTION
   *    MONTH                       - Either the month of DATE1, or "OVERDUE" when we are past the
   *                                  first day of this month
   *    TONNAGE                     - +/- ASSIGNMENT_QTY, depending on S.PORS
   *    ASSIGNMENT_QTY              - SA.ASSIGNMENT_QTY
   *
   */

  val fixationsQuery = """
    SELECT
        C.NAME as counterparty,
        ch.contractdate,
        S.PORS,
        s.contractno,
        s.splitno,
        PF.FIXATIONDATE,
        PF.FIXATIONQUANTITY,
        PF.IS_PRICED,
        SPD.COMPANY_CODE,
        SPD.SPLIT_ID,
        SPD.PRICING_NR,
        SPD.QTY,
        SPD.MATERIALCODE,
        SPD.MONTH,
        SPD.YEAR,
        SPD.QPSTART,
        SPD.QPEND,
        SPD.PRICINGQTY,
        SPD.PRICED_QTY,
        SPD.CURRENT_AUDIT_ID,
        SPD.CURRENCY_CODE,
        SPD.MKT_CURRENCY,
        SPD.CONTRA_AVERAGE,
        SPD.QTY,
        SPD.NROFDAYS,
        SPD.MONTHLY_AVERAGE,
        SPD.PRICINGTYPE as PRICING_TYPE_NUMBER,
        SPD.MARKET as EXCHANGE
      FROM
        live.SPLIT_PRICING_DETAIL SPD
      LEFT JOIN
        live.PRICE_FIXATIONS PF
      ON
        ((SPD.COMPANY_CODE = PF.COMPANY_CODE) AND
         (SPD.SPLIT_ID = PF.SPLIT_ID) AND
         (SPD.PRICING_NR = PF.PRICING_NR) AND
         (SPD.DETAILNR = PF.PRICING_DETAIL_NR))
      LEFT JOIN
        live.SPLIT S
      ON
        ((S.COMPANY_CODE = SPD.COMPANY_CODE) AND
         (S.SPLIT_ID = SPD.SPLIT_ID))
      LEFT JOIN
        live.CLIENT C
      ON
        ((S.COMPANY_CODE = C.COMPANYCODE) AND
         (S.COUNTERPARTY = C.CODE))
      left join
        live.contract_header ch
      on
        s.contractno = ch.contractno
      WHERE
        ((SPD.COMPANY_CODE = 'BV') OR
        (SPD.COMPANY_CODE = 'AG'))
      AND
        S.STATUS = 'O'
      AND
        PF.IS_PRICED = 'N'

  """

  private def unsupported = new IllegalStateException("Fixations don't fit into the one trade per row scheme assumed by parent class")
  protected def createNullTrade(rs: RichResultSetRow) = {
    val splitID = TradeID(rs.getString("SPLIT_ID"), RefinedFixationTradeSystem)

    val contractDate = rs.getDay("CONTRACTDATE")
    val counterparty = rs.getString("COUNTERPARTY")

    // TODO [11 Aug 2010] add month in extraColumns - and allocated also
    val groupCompany = rs.getString("COMPANY_CODE")
    val exchange = rs.getString("EXCHANGE")
    val pricingType = (rs.getInt("PRICING_TYPE_NUMBER"), rs.getString("MONTHLY_AVERAGE")) match {
      case (2, _) => "Unknown"
      case (_, "N") => "Partial Average"
      case (_, "Y") => "Average"
    }
    val contractNo = rs.getString("CONTRACTNO")

    val attributes = RefinedFixationTradeAttributes(
      groupCompany,
      exchange,
      contractNo,
      pricingType)

    new Trade(splitID, contractDate, counterparty, attributes, NullInstrument())
  }

  private def allTrades(sql : String)(f : (Trade) => Unit) : (Int, Set[String]) = {
    var uniqueErrors = Set[String]()
    var errorCount = 0
    val tradeFixations = scala.collection.mutable.Map[Trade, List[RefinedFixation]]()
    externalDB.query(sql, Map[String, Any]()){
      rs => {
        try {
          val trade = createNullTrade(rs)

          try {
            val fixation = RefinedFixationReader.create(rs)
            tradeFixations(trade) = fixation :: (tradeFixations.getOrElse(trade, List[RefinedFixation]()))
          } catch {
            case e => {
              if (!uniqueErrors.contains(e.toString)) {
                Log.warn("Error Trade: " + e + " " + trade.tradeID + " " + trade.tradeDay)
                uniqueErrors = uniqueErrors + e.toString
              }
              errorCount += 1
              ErrorInstrument(StackTraceToString.string(e).trim)
            }
          }
        }
        catch {
          case e => Log.error("Problem creating empty trade: " + rs, e)
        }
      }
    }
    tradeFixations.foreach{
      case (trade, fixationsList) =>
        f(trade.copyWithInstrument(RefinedFixationsForSplit(fixationsList)))
    }
    (errorCount, uniqueErrors)
  }

  def allTrades(f: (Trade) => Unit) : (Int, Set[String]) = allTrades(fixationsQuery)(f)

  def trade(tradeID: String)(f: (Trade) => Unit) {
    allTrades(fixationsQuery + " SPD.SPLIT_ID = " + tradeID)(f)
  }

  def readers = throw unsupported
}