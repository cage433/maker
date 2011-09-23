package starling.neptune

import instrumentreaders.RefinedAssignmentReader
import java.lang.String
import starling.quantity.{UOM, Quantity}
import starling.instrument._
import starling.richdb.{RichDB, RichResultSetRow}
import starling.db.{RefinedAssignmentTradeSystem}
import starling.market.{Commodity, NeptunePricingExchange, FuturesMarket}
import starling.utils.Reflection
import starling.instrument.{Trade, TradeID}
import starling.systemofrecord.{SystemOfRecordBackedByADatabase, InstrumentReader, SystemOfRecord}

class RefinedAssignmentSystemOfRecord(externalDB : RichDB) extends SystemOfRecordBackedByADatabase(externalDB){
  /**
   * In the 'Risk by Month' sheet we need to group by
   *
   *    BV                          - SA.COMPANY_CODE
   *    COMMODITY ME_SP_TRANSFER    - Commodity + Market - e.g. Copper(LME).
   *                                  This is simply the instrument's futures market
   *    COMMODITY                   - Lookup from M.DESCRIPTION
   *    RISK MARKET                 - if 'Arb Intention' - SA.ARB_IND is 'Y'
   *                                    Map(CMX -> LME, LME -> CMX)(EXCHANGE)
   *                                  else
   *                                    EXCHANGE
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
   * For the trade we need
   *
   *    Assignment ID               - SA.ASSIGNMENT_NO
   *
   *    Contract No                 - S.CONTRACT_NO
   */

  val assignmentsQuery =
   """
     SELECT

       BP.BENCHMARK_PRICE,

       C.DESCRIPTION as COMMODITY_CATEGORY,

       CL.DESCRIPTION as CONTRACT_LOCATION_DESCRIPTION,

       CLI.NAME as COUNTERPARTY,

       COU.NAME as RISK_AREA,

       GL.DESCRIPTION as HUB,

       LO.DESCRIPTION as LOCATION_DESCRIPTION,

       M.DESCRIPTION as COMMODITY_DESCRIPTION,

       PR.PARITY_RATE,

       S.CONTANGO,
       S.CONTRACT_CURRENCY,
       S.CONTRACTNO,
       S.DELDATE AS 'CONTRACTUAL DATE',
       S.EST_DEL_DATE as SPLIT_EST_DEL_DATE,
       S.FX_RATE,        
       S.MARKET AS EXCHANGE,
       S.MARKET_CURRENCY,
       S.PORS,
       S.PRICING_SPREAD_PRICE,
       S.SPLITNO,
       S.STATUS,

       SA.ACTIVE,
       SA.ARB_IND,
       sa.assignment_date,
       SA.ASSIGNMENT_NO,
       SA.ASSIGNMENT_QTY,
       SA.ASSIGNMENT_SPREAD_PRICE,
       SA.COMPANY_CODE,
       SA.CONTACT_NAME,
       SA.CONT_TERMS_CODE,
       SA.DEST_TERMS_CODE,
       SA.DIS_PORT_CODE,
       SA.EST_DEL_DATE as 'ASSIGNMENT_EST_DEL_DATE',
       SA.EST_DEL_MONTH,
       SA.EST_DEL_YEAR,
       SA.IS_ASSIGNED,
       SA.IS_DELETED,
       SA.LOAD_PORT_CODE,
       SA.SPLIT_ID,
       SA.TRADER,
       SA.TRAFFIC,
       SA.TRANSPORT_IND,
       SA.VOYAGE_ID,

       SAD.ALLOCATION_NO,

       SPD.CURRENCY_CODE,
       SPD.FIXED_PRICE AS 'PREMIUM',
       SPD.PRICE_UNIT,

       SUM_STA.BOL_DATE,
       SUM_STA.NETT_WEIGHT
     FROM
       live.SPLIT_ASSIGNMENT SA
     LEFT JOIN
       live.SPLIT S
     ON
       ((SA.COMPANY_CODE = S.COMPANY_CODE) AND
        (SA.SPLIT_ID = S.SPLIT_ID))
     LEFT JOIN
       live.SPLIT_ALLOCATION_DETAIL SAD
     ON
       ((SAD.COMPANY_CODE = SA.COMPANY_CODE) AND
         (SAD.ASSIGNMENT_NO = SA.ASSIGNMENT_NO) AND
       (SAD.IS_DELETED = 'N'))
     LEFT JOIN
        live.MATERIAL M
     ON
       M.CODE = SA.MATERIAL_CODE
     LEFT JOIN
        live.CATEGORY C
     ON
       C.CODE = SA.CATEGORY_CODE
     LEFT JOIN
        live.CONTRACT_LOCATION CL
     ON
       CL.CODE = SA.CONT_TERMS_LOCATION
     LEFT JOIN
        live.CLIENT CLI
     ON
       ((CLI.CODE = S.COUNTERPARTY) AND
       (CLI.COMPANYCODE = S.COMPANY_CODE))
         LEFT JOIN
            live.LOCATION LO
         ON
           SA.DEST_TERMS_LOCATION = LO.CODE
         LEFT JOIN
           live.COUNTRY COU
         ON
           LO.COUNTRYCODE = COU.CODE
         LEFT JOIN
           live.LOCATION_AREA_LINK LAK
         ON
           LAK.COUNTRY_CODE = COU.CODE
         LEFT JOIN
           live.GEOGRAPHICAL_LOCATION GL
         ON
           LAK.AREA_CODE = GL.CODE
         LEFT JOIN
           live.BENCHMARKPRICES BP
         ON
           ((BP.CATEGORY_CODE = SA.CATEGORY_CODE) AND
           (BP.LOCATION_CODE = GL.CODE) AND
           (BP.MATERIAL_CODE = SA.MATERIAL_CODE))
         LEFT JOIN
            live.PARITY_RATES PR
         ON
           ((SA.CONT_TERMS_CODE = PR.CONT_TERM_CODE) AND
            (SA.CONT_TERMS_LOCATION = PR.CONT_LOC_CODE) AND
            (SA.DEST_TERMS_CODE = PR.DEL_TERM_CODE) AND
            (LO.COUNTRYCODE = PR.DEL_AREA_CODE))
         LEFT JOIN
            live.SPLIT_PRICING_DETAIL SPD
         ON
           ((S.COMPANY_CODE = SPD.COMPANY_CODE) AND
            (SPD.SPLIT_ID = S.SPLIT_ID) AND
            (SPD.DETAILTYPE = 'F') AND
            (SPD.PRICING_NR = 1))
         LEFT JOIN (
           SELECT
             STA.PURCHASE_SPLIT_ID,
             STA.ASSIGNMENT_NO,
             MAX(STA.BOL_DATE) as 'BOL_DATE',
             SUM(STA.NETT_WEIGHT) as 'NETT_WEIGHT'
           FROM live.STOCK_AUDIT STA
           WHERE STA.IS_ACTIVE = 'Y'
           GROUP BY STA.PURCHASE_SPLIT_ID, STA.ASSIGNMENT_NO
           ) AS SUM_STA
         ON
           ((SA.SPLIT_ID = SUM_STA.PURCHASE_SPLIT_ID) AND
           (SA.ASSIGNMENT_NO = SUM_STA.ASSIGNMENT_NO))


         WHERE
           ((SA.COMPANY_CODE = 'BV') OR
           (SA.COMPANY_CODE = 'AG'))
         AND
           SA.IS_DELETED = 'N'
         AND
           S.STATUS = 'O'

       """


  protected def createNullTrade(rs: RichResultSetRow) = {
    val assignmentID = TradeID(rs.getString("ASSIGNMENT_NO"), RefinedAssignmentTradeSystem)
    val assignmentDay = rs.getDay("assignment_date")
    val counterparty = rs.getString("COUNTERPARTY")


    val groupCompany = rs.getString("COMPANY_CODE")
    val exchange = rs.getString("EXCHANGE")
    val hub = rs.getString("HUB")
    val commodityCategory = rs.getStringOrBlank("COMMODITY_CATEGORY")
    // TODO [11 Aug 2010] add month in extraColumns - and allocated also
    val contractNo = rs.getString("CONTRACTNO")
    val allocationNo = rs.getStringOrBlank("ALLOCATION_NO")
    val riskArea = rs.getStringOrBlank("RISK_AREA")

    val attributes = RefinedAssignmentTradeAttributes(
      groupCompany,
      exchange,
      hub,
      commodityCategory,
      contractNo,
      allocationNo,
      riskArea)

    new Trade(assignmentID, assignmentDay, counterparty, attributes, NullInstrument())
  }
  
  def allTrades(f: (Trade) => Unit) = {
    allTrades(assignmentsQuery, Map[String, Any]())(f)
  }

  def trade(tradeID: String)(f: (Trade) => Unit) = allTrades( assignmentsQuery + " and ASSIGNMENT_NO = " + tradeID, Map[String, Any]())(f)

  val readers = List(RefinedAssignmentReader)
}


