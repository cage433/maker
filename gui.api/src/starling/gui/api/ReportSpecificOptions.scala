package starling.gui.api

class ReportSpecificOptions(val options : List[(String, List[Any])]) {
  def this(seqOptions : (String, List[Any])*) = this(seqOptions.toList)

  def :+(option : (String, List[Any])) = new ReportSpecificOptions(options :+ option)

  def ++(right : ReportSpecificOptions) : ReportSpecificOptions =
    new ReportSpecificOptions(options ++ right.options)

  def distinct : ReportSpecificOptions = new ReportSpecificOptions(options.distinct)

  def default = options.map{ case (label, choices) => label -> choices.head}.toMap

  def stringValues = options.map {
    case (name,values) => {
      name -> values.map { v =>
        v match {
          case true|false => v
          case _ => v.toString
        }
      }
    }
  }

  def labels = options.map(_._1)

  def valuesFor(label : String): Option[scala.List[Any]] = options.toMap.get(label)
  def filter(validOptions : List[(String, List[Any])]) = new ReportSpecificOptions(options.filter(validOptions.contains))
  def containsKey(key : String) = options.exists(_._1 == key)
}

object ReportSpecificOptions{
  val MonthChoiceText = "M"
  val WeekChoiceText = "W"
  val DayChoiceText = "D"
  val defaultLabel: String = "Default"
  val positionLabel: String = "Position"
  val barrelLabel: String = "Barrel"
  val tonneLabel: String = "Tonne"
  val cubicMetreLabel = "<html>m<sup>3</sup></html>"
  val positionOnlyLabel = "Position Only"
  val quotedLabel = "Quoted"
  val utpLabel: String = "UTP"
  val usdLabel: String = "USD"
  val eurLabel: String = "EUR"
  val cnyLabel: String = "CNY"
  val cnhLabel: String = "CNH"
  val riskTypeLabel: String = "Risk Type"
  val riskCommodityLabel: String = "Risk Commodity"
  val riskMarketLabel: String = "Risk Market"
  val riskPeriodLabel: String = "Risk Period"
  val riskVolumeLabel: String = "Risk Volume"
  val errorLabel: String = "Error"
  val strategyLabel = "Strategy"
  val tradeIDLabel = "Trade ID"



  val collapseOptions @ (collapseOptionsLabel, collapseOptionsChoices) = ("Collapse Options", List(true, false))
  val showEqFutures @ (showEqFuturesLabel, showEqFuturesChoices) = ("Show Eq Futures", List(false, true))
  val futuresAsSpreads @ (futuresAsSpreadsLabel, futuresAsSpreadsChoices) = ("Futures as spreads", List(false, true))
  val futuresAsSwaps @ (futuresAsSwapsLabel, futuresAsSwapsChoices) = ("Futures as swaps", List(false, true))
  val useSkew @ (useSkewLabel, useSkewChoices) = ("Skew", List(true, false))
  val oilTenors @ (tenorLabel, oilTenorChoices) = ("Tenor", List(MonthChoiceText, WeekChoiceText, DayChoiceText))
  val metalsTenors @ (_, metalsTenorChoices) = (tenorLabel, List(MonthChoiceText, DayChoiceText))
  val atmVega @ (atmVegaLabel, atmVegaChoices) = ("ATM Vega", List(false, true))
  val positionType @ (positionTypeLabel, positonTypeChoices) = ("Position", List(defaultLabel, barrelLabel, tonneLabel, usdLabel))
  val priceUnit @ (priceUnitLabel, priceUnitChoices) = ("Price Unit", List(positionLabel, quotedLabel))
  val lots @ (lotsLabel, lotsChoices) = ("Lots", List(false, true))
  val valuationCurrency @ (valuationCurrencyLabel, valuationCurrencyChoices) = ("CCY", List(defaultLabel, usdLabel, cnyLabel, cnhLabel, eurLabel))
}

