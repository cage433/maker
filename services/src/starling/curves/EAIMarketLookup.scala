package starling.curves

import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.market.formula._
import starling.quantity.Conversions
import starling.quantity.UOM._
import starling.market._
import rules.{PrecisionRules, MarketPrecisionFactory, Precision}
import starling.calendar.{BusinessCalendar, HolidayTablesFactory, BusinessCalendars}
import starling.daterange.{Day, Month}

class EAIMarketLookup(eai: DB, expiryRules: FuturesExpiryRules) extends MarketLookup {
  /**
   * map of eaiquoteid to index
   */
  lazy val table: List[Either[CommodityMarket, Index]] = load

  protected lazy val allMarketsImpl = table.flatMap {
    case Left(m: FuturesMarket) => Some(m)
    case _ => None
  }
  lazy val allIndexes = table.flatMap {
    case Right(m) => Some(m)
    case _ => None
  }

  lazy val cals: BusinessCalendars = new BusinessCalendars(HolidayTablesFactory.holidayTables)
  lazy val expiry: FuturesExpiryRules = FuturesExpiryRuleFactory.expiryRules
  lazy val precisionRules: PrecisionRules = MarketPrecisionFactory.rules

  // EAI has duplicate incorrect quotes that they don't want to remove (even though they're unused and wrong)
  val exludeIDs = List(137, 174, 756, 759)
  // ids that are broken for various reasons
  val brokenIDS = List(
    1123, 1124, 1138, 1139, 1442, 1443, 1534, 1535 // cbot soy and corn, all have conversions to bbl, don't make sense and we don't trade them yet in Starling
  )

  private def load: List[Either[CommodityMarket, Index]] = {
    var all = List[Either[CommodityMarket, Index]]()
    eai.query((
      select("*, u.name as units, pc.name as productcategory, its.name as instrumentType")
        from ("tblquotes q")
        leftJoin ("tblCurrencies c", ("q.currencyid" eql "c.id"))
        leftJoin ("tblCalendars cal", ("cal.ID" eql "q.CalendarID"))
        leftJoin ("tblFCcalendars h", ("h.id" eql "cal.FCCalendarID"))
        leftJoin ("tblProductCategories pc", ("pc.id" eql "q.ProductCategoryID"))
        leftJoin ("tblUnits u", ("u.id" eql "q.unitid"))
        leftJoin ("tblQuoteSymbols qs", ("qs.id" eql "q.quoteSymbolID"))
        leftJoin ("tblInstrumentTypes its", ("its.id" eql "q.instrumentTypeID"))
        where (
        ("productCategoryID" gt 0) // we don't want markets with no commodity as they're just made up.
          and
          ("productCategoryID" lt 15) // we don't want currencies and emissions
          and
          ("u.name" isNotNull) // stuff like CDSs have a null unit|
          and
          ("q.id" notIn exludeIDs)
          and
          ("q.id" notIn brokenIDS)
        )
        orderBy ("q.id" asc)
      )) {
      rs => {
        val name = rs.getString("longname")
        val nameLower = name.toLowerCase
        val quoteId = rs.getInt("id")

        val underlying: Option[Either[CommodityMarket, Index]] = try {
          val ccy = rs.getUOM("symbol")
          val (uom, lotSize) = if (nameLower.contains("henry hub")) {
            (MMBTU, Some(10000.0))
          } else {
            val lots = rs.getDouble("lotSize") match {
              case 0 => None
              case l => Some(l)
            }
            (rs.getUOM("units"), lots)
          }

          val default = rs.getInt("defaultPrecision")
          val clearport = rs.getInt("clearportdefaultprecision")
          val precision = Some(Precision(default, clearport))
          val limMultiplier = rs.getDouble("LIMPriceMultiplier")
          val limSymbol = rs.getStringOption("LimSymbol").map(LimSymbol(_, limMultiplier))
          val calendarCode = rs.getString("calendarCode")
          val calendar = cals.financialHolidaysOption(calendarCode) match {
            case Some(c) => c
            case None if calendarCode != null => BusinessCalendar.error(calendarCode)
            case _ => BusinessCalendar.NONE
          }

          val product = rs.getString("productcategory")
          val commodity = getCommodity(nameLower, product)
          val conversion: Conversions = rs.getDouble("conversionFactor") match {
            case 0 => Conversions.default
            case v => commodity match {
              case _: OilCommodity => Conversions.default + ((BBL / MT, v))
              case _: Gas => Conversions.default + ((BBL / MT, v))
              case _ => Conversions.default
            }
          }

          val futureID = rs.getInt("underlyingFutureID")

          val quote: Some[Either[CommodityMarket, Index] with Product] = rs.getString("instrumentType") match {
            case "Future" => {
              val volID = rs.getInt("volatilityID") match {
                case 0 => None
                case v => Some(v)
              }
              val exchange = getExchange(name)
              val ruleOrEmptyRule = {
                exchange match {
                  case FuturesExchangeFactory.LME => expiryRules.LME
                  case FuturesExchangeFactory.BALTIC => expiryRules.BALTIC
                  case FuturesExchangeFactory.COMEX if nameLower.contains("gold") => expiryRules.COMEX_G_S_HG_COPPER
                  case FuturesExchangeFactory.COMEX if nameLower.contains("hg copper") => expiryRules.COMEX_G_S_HG_COPPER
                  case FuturesExchangeFactory.COMEX if nameLower.contains("silver") => expiryRules.COMEX_G_S_HG_COPPER
                  case FuturesExchangeFactory.COMEX if nameLower.contains("um") => expiryRules.COMEX_PT_PA
                  case FuturesExchangeFactory.SFS if nameLower.contains("fuel") => expiryRules.SHANGHAI_FUEL_OIL
                  case FuturesExchangeFactory.SFS => expiryRules.SHANGHAI
                  case _ => expiry.ruleOrEmptyRule(quoteId, name)
                }
              }
              val tenor = exchange match {
                case FuturesExchangeFactory.LME => Day
                case _ => Month
              }

              val futuresMarket = new FuturesMarket(name, lotSize, uom, ccy, calendar, Some(quoteId), tenor, ruleOrEmptyRule, exchange, commodity, conversion, volID, limSymbol, precision)

              // this blows up for soybeans:
              futuresMarket.standardShift

              Some(Left(futuresMarket: CommodityMarket))
            }
            case "Spot Future" => {
              if (futureID != 101 && futureID != 149) {
                // platts brent 1st month etc. has 101 and is wrong
                val future = all.find(_ match {
                  case Left(m: FuturesMarket) => m.eaiQuoteID == Some(futureID)
                  case _ => false
                })
                val rollbeforedays = rs.getInt("rolloverbeforedays")
                val promptness = rs.getInt("nearbymonthno")
                val market = future match {
                  case Some(Left(market: FuturesMarket)) => market
                  case _ => throw new Exception("Couldn't find future for " + (name, quoteId) + " with underlying of " + futureID)
                }
                Some(Right(new FuturesFrontPeriodIndex(name, Some(quoteId), market, rollbeforedays, promptness, precision)))
              } else {
                // platts brent 1st month, 2nd ...
                // platts wti 1st month¸ 2nd ..
                // TODO this is wrong
                val level = rs.getStringOption("LimColumn").map(Level.fromName).getOrElse(Level.Close)
                Some(Right(new PublishedIndex(name, Some(quoteId), lotSize, uom, ccy, calendar, commodity, conversion, limSymbol, precision, level)))
              }
            }
            case "Spot" | "Spot Periodical" | "Freight" | "Diff" | "Pipeline" => {
              val formulaID = rs.getInt("formulaID")
              val level = rs.getStringOption("LimColumn").map(Level.fromName).getOrElse(Level.Close)
              Some(Right(formulaID match {
                case 0 => {
                  new PublishedIndex(name, Some(quoteId), lotSize, uom, ccy, calendar, commodity, conversion, limSymbol, precision, level)
                }
                case _ => {
                  getFormula(formulaID) match {
                    case Some(formula) => {
                      new FormulaIndex(name, new Formula(formula), ccy, uom, precision, Some(conversion), Some(quoteId))
                    }
                    case _ => {
                      println("Invalid formula with id: " + formulaID)
                      throw new Exception("Invalid formula with id: " + formulaID)
                    }
                  }
                }
              }))
            }
          }
          quote
        } catch {
          case e => {
            println("failed with market " + (name, quoteId) + ", " + e)
            None
          }
        }

        underlying.map(all ::= _)
      }
    }
    all.reverse
  }

  lazy val metalCommodities = ("NASAAC", NASAAC) :: ("Plantinum", Platinum) :: Commodity.metalsCommodities.map(c => (c.name.split("(?<!^)(?=[A-Z])").mkString(" "), c))

  def getCommodity(name: String, product: String) = {
    name match {
      case "nymex wti" | "ice wti" => WTI
      case "ipe brent" | "nymex brent" => Brent
      case "nymex heat" | "ice heat" => HeatingOil
      case _ => {
        if (product == "Metal") {
          metalCommodities.filter{case (cname, _) => name.contains(cname.toLowerCase)} match {
            case (_, comm) :: Nil => comm
            case Nil =>               throw new Exception("no match for : " + product)

            case e => e.sortWith(_._1.length > _._1.length).head._2
          }
        } else {
          Commodity.fromNameOption(product.replace("Jet / Kero", "JetKero").replace(" ", "")) match {
            case Some(c) => c
            case None => {
              println("???")
              throw new Exception("no match for : " + product)
            }
          }
        }
      }
    }
  }


  def getFormula(formulaID: Int): Option[String] = formulaID match {
    case 0 => Some("")
    case _ => {
      eai.queryWithOneResult((
        select("*")
          from ("tblformulae")
          where ("id" eql formulaID)
        )) {
        rs => {
          val eaiQuoteID = rs.getInt("quoteid")
          val coeff = rs.getDouble("coefficient")
          val nextFormulaID = rs.getString("nextid") match {
            case null | "" => 0
            case s => s.toInt
          }
          val op = rs.getString("operator") match {
            case "+" => "+"
            case "-" => "-"
            case "" | null => ""
            case u => throw new Exception("Uknown op: '" + u + "'")
          }

          getFormula(nextFormulaID).map {
            f => "" + coeff + "* MKT(" + eaiQuoteID + ")" + op + " " + f
          }
        }
      }.getOrElse(None)
    }
  }

  def getExchange(marketName: String) = {
    import FuturesExchangeFactory._
    marketName.split(" ").head.toLowerCase match {
      case "nymex" => NYMEX
      case "comex" => COMEX
      case "cbot" => COMEX
      case "shfe" | "shanghai" => SFS
      case "baltic" => BALTIC
      case "ice" | "ipe" => ICE
      case "mdex" => MDEX
      case "lme" => LME
      case "dce" => DCE
      case "dme" => DME
      case "tocom" => TOCOM
      case "nyse" => TOCOM
    }
  }
}