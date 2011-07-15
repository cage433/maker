package starling.market

import formula.{Formula, FormulaIndex}
import rules.Precision
import starling.daterange.{TenorType, Month, Day, Tenor}
import starling.quantity.{Conversions, UOM}
import starling.quantity.UOM._
import starling.calendar.{BusinessCalendar, BusinessCalendars}
import starling.utils.ImplicitConversions._

class MarketParser(businessCalendars: BusinessCalendars, futuresExpiryRules: FuturesExpiryRules) {

  import MarketParser._

  def fromLines(lines: List[Line]): List[Either[Market, Index]] = {
    var all = List[Either[Market, Index]]()

    def futuresMarketFromName(marketName: String) = all.findEnsureOnlyOne {
      case Left(f: FuturesMarket) => f.name.equalsIgnoreCase(marketName)
      case _ => false
    } match {
      case Some(f) => f.left.get.asInstanceOf[FuturesMarket]
      case None => throw new Exception("No underlying futures market for " + marketName)
    }

    def futuresMarketFromID(quoteID: Int) = all.findEnsureOnlyOne {
      case Left(f: FuturesMarket) => f.eaiQuoteID == Some(quoteID)
      case Right(f: FuturesFrontPeriodIndex) => f.eaiQuoteID == Some(quoteID)
      case _ => false
    } match {
      case Some(Left(f:FuturesMarket)) => f
      case Some(Right(f:FuturesFrontPeriodIndex)) => f.market
      case None => throw new Exception("No underlying futures market for " + quoteID)
    }


    lines.map {
      line => try { {
        val name = line.get("name")
        val eaiQuoteID = line.getFromOption[Int]("eaiQuoteID")
        val className = line.get("type")

        val defaultPrec = line.getFromOption[Int]("defaultPrecision")
        val clearPortPrec = line.getFromOption[Int]("clearPortPrecision")
        val precision = (defaultPrec, clearPortPrec) match {
          case (Some(s), Some(m)) => Some(Precision(s, m))
          case _ => None
        }

        val entries: List[Either[Market, Index]] = className match {
          case "FuturesMarket" | "PublishedIndex" => {
            val uom = line.getUOM("uom")
            val ccy = line.getUOM("ccy")
            val lotSize = line.getFromOption[Double]("lotSize")
            val businessCalendarString = line.get("businessCalendar")
            val calendar = businessCalendars.financialHolidaysOption(businessCalendarString) match {
              case Some(c) => c
              case None if businessCalendarString != "null" => BusinessCalendar.error(businessCalendarString)
              case _ => BusinessCalendar.NONE
            }
            val tenor = TenorType.parseTenorName(line.get("tenor"))
            val commodity = Commodity.fromName(line.get("commodity"))

            val limSymbol = line.getFromOption[String]("limSymbol")
            val limMultiplier = line.getFromOption[Double]("limMultiplier")
            val lim = (limSymbol, limMultiplier) match {
              case (Some(s), Some(m)) => Some(LimSymbol(s, m))
              case _ => None
            }

            val conversion = line.getFromOption[Double]("bblPerMt") match {
              case Some(c) => Conversions.default + (BBL / MT, c)
              case None => Conversions.default
            }

            className match {
              case "FuturesMarket" => {
                val expiryRule = futuresExpiryRules.fromName(line.get("expiryRule")) match {
                  case Some(r) => r
                  case None => throw new Exception("No rule for " + line.get("expiryRule"))
                }
                val exchangeName = line.get("exchange")
                val exchange = FuturesExchangeFactory.fromName(exchangeName)
                val volatilityID = line.getFromOption[Int]("volatilityID")

                val futuresMarket = FuturesMarket(name, lotSize, uom, ccy, calendar, eaiQuoteID, tenor, expiryRule, exchange, commodity, conversion, volatilityID, lim, precision)
                List(Left(futuresMarket))
              }
              case "PublishedIndex" => {
                val level = line.get("indexLevel") match {
                  case "" => Level.Close
                  case s => Level.fromName(s)
                }

                List(Right(new PublishedIndex(name, eaiQuoteID, lotSize, uom, ccy, calendar, commodity, conversion, lim, precision, level)))
              }
            }
          }
          case "FormulaIndex" | "FormulaIndex/FuturesSpreadMarket" => {
            val uom = line.getUOM("uom")
            val ccy = line.getUOM("ccy")
            val formula = new Formula(line.get("formula"))
            val conversion = line.getFromOption[Double]("bblPerMt") match {
              case Some(c) => Some(Conversions.default + (BBL / MT, c))
              case None => None
            }
            val index = new FormulaIndex(name, formula, ccy, uom, precision, conversion, eaiQuoteID)

            val lotSize = line.getFromOption[Double]("lotSize")
            if (lotSize.isDefined) {
              val tenor = TenorType.parseTenorName(line.get("tenor"))

              val MarketParser = """1\.0[ ]*\*[ ]*MKT\((\d+)\)[ ]*\-[ ]*1\.0[ ]*\*[ ]*MKT\((\d+)\)""".r
              val MarketParser(mkt1, mkt2) = formula.formula
              val market1 = futuresMarketFromID(mkt1.toInt)
              val market2 = futuresMarketFromID(mkt2.toInt)

              val (expiryRule, hasOptions) = line.getFromOption[String]("expiryRule") match {
                case Some(ruleName) => (futuresExpiryRules.fromName(ruleName).get, true)
                case _ => (FuturesSpreadMarket.defaultExpiryRule(market1, market2), false)
              }

              val futuresSpread = new FuturesSpreadMarket(name, uom, ccy, market1, market2, lotSize, tenor, eaiQuoteID,
                expiryRule, FuturesSpreadMarket.NoExchangeMonthly, hasOptions)

              List(Left(futuresSpread), Right(index))
            } else {
              List(Right(index))
            }
          }
          case "FuturesFrontPeriodIndex" => {
            val rollbeforedays = line.getInt("rollbefore")
            val promptness = line.getInt("promptness")
            val marketName = line.get("forwardMarket")
            val market = futuresMarketFromName(marketName)
            List(Right(new FuturesFrontPeriodIndex(name, eaiQuoteID, market, rollbeforedays, promptness, precision)))
          }
        }
        all :::= entries
      }
      }
      catch {
        case e => throw new Exception("Failed while parsing: " + line, e)
      }
    }
    all
  }
}

object MarketParser {

  trait Line {
    def get(name: String): String

    def getInt(name: String) = get(name).toInt

    val OptionRegex = """Some\((.+)\)""".r

    def getFromOption[T](name: String)(implicit m: Manifest[T]): Option[T] = get(name) match {
      case "" => None
      case "None" => {
        None
      }
      case OptionRegex(value) => {
        m.toString match {
          case "Int" => Some(value.toInt).asInstanceOf[Option[T]]
          case "Double" => Some(value.toDouble).asInstanceOf[Option[T]]
          case "java.lang.String" => Some(value).asInstanceOf[Option[T]]
        }
      }
    }

    def getUOM(name: String): UOM = UOM.fromString(get(name))
  }

}