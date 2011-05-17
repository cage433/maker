package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.daterange._
import java.text.DecimalFormat
import org.springframework.core.io.ByteArrayResource
import org.springframework.mail.javamail.{JavaMailSender, MimeMessageHelper}
import starling.rmi.{UserReportsService}
import starling.auth.User
import starling.props.{PropsHelper}
import starling.pivot.model.{TotalAxisValueType, AxisValue, MeasureAxisValueType}
import starling.loopyxl.ExcelMethod
import starling.quantity.{UOM, Quantity}
import collection.immutable.TreeMap
import starling.market.{EquityPrices, EquityPricesMarketDataKey, RIC}
import starling.marketdata.{SpotFXData, SpotFXDataKey}
import starling.db.{MarketDataEntry, MarketDataStore, MarketDataSet}
import starling.pivot._

/**
 * The handler for the very custom excel functions used by the special sits spreadsheet
 */
class SpecialSitsHandler(userReportsService : UserReportsService,
                    marketDataStore : MarketDataStore,
                    sender: JavaMailSender) {

  @ExcelMethod
  @XLFunction(
    name = "reportErrors",
    category = "Starling",
    args = Array("user name", "report name", "observation day"))
  def reportErrors(
                    userName: String,
                    reportName: String,
                    observationDate: Double): Array[String] = {
//    val reportParameters = createReportParameters(userName, reportName, observationDate)
//    val errors = starlingServer.reportErrors(reportParameters)
//    val headingCell = if (errors.errors.isEmpty) "No Errors" else errors.errors.size + " errors"
//    (headingCell :: errors.errors.take(20).map(_.message)).toArray
    Array("")
  }

  def specialSitsRunReport(
                 userName:String,
                 reportName: String,
                 observationDate: Double,
                 optionalLayoutName: String): Array[Array[Any]] = {
    val layout = if (optionalLayoutName == null) None else Some(optionalLayoutName)
    val day = Day.fromExcel(observationDate)
    val pivotTable = userReportsService.runNamedReport(User(userName), reportName, day, layout).get

    if (reportName == "SpecialSitsVAR-spreadsheet") {
      sendSpecialSitsEmail(User(userName), reportName, day)
    }

    val format = new DecimalFormat("#######0.00")
    val arrays = pivotTable.toFlatRows(Totals.Null).map(_.toArray).toArray

    //Hack so that single value reports return just the number
    if (pivotTable.rowAxis.length == 1 && pivotTable.columnAxis.length == 1) {
      Array(Array(arrays(1)(1)))
    } else {
      arrays
    }
  }

  @XLFunction(
    name = "uploadFXHistory",
    category = "Starling",
    args = Array("label", "priceHistory"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "A range containg the fx history with Currency at the top and days on the left"))
  def uploadFXHistory(label: String, priceHistory: Array[Array[Object]]) = {
    if (label == null) throw new Exception("Label is null")
    val ccyHeader = Map() ++ priceHistory(0).toList.tail.zipWithIndex.flatMap{
      case (ccy, index) => {
        ccy match {
          case "" => None
          case s: String => Some((index + 1) -> UOM.fromIdentifier(s.replaceAll("=", "").trim)) //+1 beacuse Day is column 0
          case _ => None
        }
      }
    }
    val processedRates = priceHistory.toList.tail.flatMap{
      row => {
        row.head match {
          case d: java.lang.Double => {
            val observationPoint = ObservationPoint.fromExcel(d.doubleValue)
            ccyHeader.flatMap{
              case (index, ccy) => {
                row(index) match {
                  case d: java.lang.Double => {
                    val value = d.doubleValue
                    val rateInUSD = ccy match {
                      case UOM.GBP | UOM.AUD => true; case _ => false
                    }
                    val rate = if (rateInUSD) {
                      Quantity(value, UOM.USD / ccy)
                    } else {
                      Quantity(value, ccy / UOM.USD).invert
                    }
                    Some((observationPoint, ccy, rate))
                  }
                  case _ => None
                }
              }
            }
          }
          case _ => None
        }
      }
    }
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(Map(marketDataSet -> processedRates.map{
      case (observationDay, ccy, spotFX) => {
        MarketDataEntry(observationDay, SpotFXDataKey(ccy), SpotFXData(spotFX))
      }
    }))
    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadEquityPrices",
    category = "Starling",
    args = Array("label", "observationDay", "rics", "prices"))
  def uploadEquitiesHistory(label: String, observationDate: Double, rics: Array[Object], prices: Array[Object]) = {
    val observationPoint = ObservationPoint.fromExcel(observationDate)
    if (rics.size != prices.size) {
      throw new Exception("The ric and price ranges must be the same size")
    }
    val equityPrices = EquityPrices(TreeMap.empty[RIC, Quantity](RIC.ordering) ++ (rics zip prices).flatMap{
      case (ricOrBlank, priceOrBlank) => {
        (ricOrBlank, priceOrBlank) match {
          case (null, null) => None
          case ("", _) => None
          case (_, d: java.lang.Double) if d == 0 => None
          case (s: String, d: java.lang.Double) => {
            val ric = RIC(s)
            val price = {
              val value = d.doubleValue
              if (ric.currency == UOM.GBP) {
                value / 100
              } else {
                value
              }
            }
            Some(ric -> Quantity(price, ric.currency / UOM.SHARE))
          }
          case (_, _) => None
        }
      }
    })
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(marketDataSet, observationPoint, EquityPricesMarketDataKey, equityPrices)
    "OK:" + result
  }


  @ExcelMethod
  @XLFunction(
    name = "uploadEquitiesHistory",
    category = "Starling",
    args = Array("label", "priceHistory"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "A range containg the price history with RICs at the top and days on the left"))
  def uploadEquitiesHistory(label: String, priceHistory: Array[Array[Object]]) = {
    if (label == null) throw new Exception("Label is null")
    val ricHeader = Map() ++ priceHistory(0).toList.tail.zipWithIndex.flatMap{
      case (ricOrCurrency, index) => {
        ricOrCurrency match {
          case "" => None
          case s: String => Some((index + 1) -> RIC(s)) //+1 beacuse Day is column 0
          case _ => None
        }
      }
    }
    val processedPrices = priceHistory.toList.tail.flatMap{
      row => {
        row.head match {
          case d: java.lang.Double => {
            val observationPoint = ObservationPoint.fromExcel(d.doubleValue)
            val equityPrices = EquityPrices(TreeMap.empty[RIC, Quantity](RIC.ordering) ++ ricHeader.flatMap{
              case (index, ric) => {
                row(index) match {
                  case d: java.lang.Double => {
                    val currency = ric.currency
                    val price = {
                      val value = d.doubleValue
                      if (currency == UOM.GBP) {
                        value / 100
                      } else {
                        value
                      }
                    }
                    Some(ric -> Quantity(price, currency / UOM.SHARE))
                  }
                  case _ => None
                }
              }
            })
            Some(MarketDataEntry(observationPoint, EquityPricesMarketDataKey, equityPrices))
          }
          case _ => None
        }
      }
    }
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(Map(marketDataSet -> processedPrices))
    "OK:" + result
  }

  private def sendSpecialSitsEmail(user:User, reportName:String, day:Day) {

    val pivotTable = userReportsService.runNamedReport(user, reportName, day, Some("Email")).get
    val theVAR = {
      val key = (
        List(AxisValue(Field("RIC"),TotalAxisValueType,0), AxisValue(Field("Strike"),TotalAxisValueType,0)),
        List(AxisValue(Field("VAR 95"),MeasureAxisValueType(Field("VAR 95")),0))
      )
      PivotFormatter.formatPivotQuantity((pivotTable.aggregatedMainBucket(key).asInstanceOf[PivotQuantity] * -1), PivotFormatter.DefaultExtraFormatInfo)
    }

    val csv = new StringBuilder()
    val format = new DecimalFormat("#######0.00")
    pivotTable.toFlatRows(Totals(false, true, false, false)).foreach{
      row => csv.append(row.map{
        value => value match {
          case tableCell: TableCell => {
            tableCell.value match {
              case p: PivotQuantity if p.hasErrors => "E"
              case p: PivotQuantity if p.values.size == 1 => format.format(p.values.toList.head._2)
              case p: PivotQuantity => "Many values"
              case d: Double => d
              case i: Int => i
              case other => tableCell.text
            }
          }
          case arr: Array[_] => arr.mkString("&")
          case _ => value.toString
        }
      }.mkString(", ") + "\n")
    }

    val dayText = day.toString("ddMMMMyyyy")

    //Hack to email the special sits VAR
    val message = sender.createMimeMessage()
    val helper = new MimeMessageHelper(message, true);
    helper.setFrom(PropsHelper.defaultProps.VarReportEmailFrom())
    helper.setTo(Array("thomas.rynne@trafigura.com", "alex.mcguire@trafigura.com", "david.corcoran@trafigura.com", "jerome.dive@trafigura.com"))
    helper.setSubject("Special Sits VAR (" + dayText + ") " + theVAR)
    helper.setText("<p>Please find attached a breakdown of the Special Sits VAR for " + day + "</p>" +
                   "<p>The combined VAR95 is " + theVAR + "</p>" +
                   "<br /><p>Thanks</p>", true)
    helper.addAttachment("SpecialSitsVAR.csv", new ByteArrayResource(csv.toString.getBytes))
    sender.send(message)
  }
}
