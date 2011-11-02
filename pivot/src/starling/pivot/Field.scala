package starling.pivot

import model.UndefinedValue

import java.io.Serializable
import starling.quantity._
import starling.utils.ImplicitConversions._
import starling.utils.{StarlingEnum}

class Field(val name: String) extends Serializable {
  override val hashCode = name.hashCode
  override def toString = "Field(" + name + ")"
  override def equals(other:Any) = {
    other match {
      case rhs:Field => name == rhs.name
      case _ => false
    }
  }
}

object Field {

  val commodity_str = "Commodity"
  val desk_str = "Desk"
  val groupCompany_str = "Group Company"
  val instrument_str = "Instrument"
  val location_str = "Location"
  val market_str = "Market"
  val portfolio_str = "Portfolio"
  val strategy_str = "Strategy"
  val trader_str = "Trader"
  val usdDelta_str = "USD Delta"
  val riskMarket_str = "Risk Market"
  val riskPeriod_str = "Risk Period"
  val name_str = "Name"
  val reportingEntity_str = "Reporting Entity"
  val scratch_str = "Scratch"
  val enteredby_str = "Entered By"
  val systemTimestamp_str = "System Timestamp"
  val dealID_str = "Deal ID"

  val exchange_str = "Exchange"
  val hub_str = "Hub"
  val commodityCategory_str = "Commodity Category"
  val contractNo_str = "Contract No"
  val allocationNo_str = "Allocation No"
  val riskArea_str = "Risk Area"

  val pricingType_str = "Pricing Type"


  val tradeCount_str = "Trade Count"

  val instrumentID_str: String = "Instrument ID"
  val utpVolume_str: String = "UTP Volume"

  val tradeID_str = "Trade ID"
  val tradeDay_str = "Trade Day"
  val counterparty_str = "Counter Party"
  val costs_str = "Costs"

  private val fieldCache = new java.util.concurrent.ConcurrentHashMap[String,Field]()

  def fromName(name:String) = apply(name)

  def apply(name:String) = {
    val field = fieldCache.get(name)
    if (field == null) {
      val f = new Field(name)
      fieldCache.put(name, f)
      f
    } else {
      field
    }
  }

  val NullField = Field("Null")
  val RootField = Field("ROOT_FIELD")

  implicit object ordering extends Ordering[Field]{
    def compare(lhs:Field, rhs:Field) : Int = lhs.name.compare(rhs.name)
  }
}

// NOTE - The implementations of Parser must be serializable (and easily serializable at that). i.e. an Object like TextPivotParser.
trait PivotParser extends Serializable {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo):(Any,String)
  def acceptableValues:Set[String] = Set.empty
}

object TextPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = (text,text)
}
object IntPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = (text.toInt, text)
}
object PivotQuantityPivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val textNoCommas = text.replaceAll(",", "").trim()
    val cleanTextMaybeWithUOM = if (textNoCommas.startsWith("(")) {
      "-" + textNoCommas.replaceAll("\\(", "").replaceAll("\\)", "")
    } else {
      textNoCommas
    }
    val letterIndex = cleanTextMaybeWithUOM.indexWhere(_.isLetter)
    if (letterIndex == -1) {
      // No UOM specified
      (PivotQuantity(cleanTextMaybeWithUOM.toDouble),"")
    } else {
      val (number, uomString) = cleanTextMaybeWithUOM.splitAt(letterIndex)
      val uom = UOM.fromString(uomString)
      val pq = PivotQuantity(new Quantity(number.toDouble, uom))
      (pq, PivotFormatter.formatPivotQuantity(pq, extraFormatInfo, false) + uom.asString)
    }
  }
}

case class DecimalPlaces(defaultFormat:String, lotsFormat:String, priceFormat:String, currencyFormat:String, percentageFormat:String) {
  def format(uom:UOM) = {
    if ((uom == UOM.K_BBL || uom == UOM.C_M3 || uom == UOM.K_MT))
      lotsFormat
    else if (uom.isCurrency)
      currencyFormat
    else if (uom.numeratorUOM.isCurrency) // probably a price
      priceFormat
    else
      defaultFormat
  }
}

object MonthFormat extends Enumeration {
  type MonthFormat = Value
  val Standard, StandardCapitalised, Short, ShortCapitalised, ShortDash, ShortDashCapitalised, Numeric, Reuters = Value
}
import MonthFormat._

case class DateRangeFormat(monthFormat:MonthFormat)

case class ExtraFormatInfo(decimalPlaces:DecimalPlaces = PivotFormatter.DefaultDecimalPlaces,
                           dateRangeFormat:DateRangeFormat = PivotFormatter.DefaultDateRangeFormat)

// Must be serializable and available on the gui (same as the PivotParsers).
trait PivotFormatter extends Serializable {
  def format(value:Any, formatInfo:ExtraFormatInfo):TableCell
}
object PivotFormatter {
  val MaxSetSize = 3

  val DefaultFormat = "#,##0"
  val PriceFormat = "#,##0.0000"
  val CurrencyFormat = "#,##0"
  val LotsFormat = "#,##0.0"
  val PercentFormat = "#0.00"

  val DefaultDateRangeFormat = DateRangeFormat(Standard)
  val DefaultDecimalPlaces = DecimalPlaces(DefaultFormat, LotsFormat, PriceFormat, CurrencyFormat, PercentFormat)
  val DefaultExtraFormatInfo = ExtraFormatInfo(DefaultDecimalPlaces, DefaultDateRangeFormat)

  def formatPivotQuantity(pq:PivotQuantity, formatInfo:ExtraFormatInfo, includeUOM:Boolean=true) = {
    import starling.utils.ImplicitConversions._
    if (pq.values.isEmpty && pq.errors.isEmpty) {
      ""
    } else if (pq.values.isEmpty) {
      if (pq.errors.size == 1) {
        "E " + pq.errors.keys.iterator.next
      } else {
        "E (" + pq.errors.size + ")"
      }
    } else {
      val warning = pq.warning.isDefined
      val l = pq.values.size - 1
      (for (((uom, value), i) <- pq.values.zipWithIndex) yield {
        val format = formatInfo.decimalPlaces.format(uom)
        val space = if (warning && i == l) false else true
        value.format(format, space)  + (if (includeUOM) uom else "")
      }).mkString(", ")
    }
  }
}

object TreePivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case pivotTreePath:PivotTreePath => new TableCell(pivotTreePath, pivotTreePath.lastElement, LeftTextPosition)
      case s:Set[_] => {
        val longText = DefaultPivotFormatter.limitedMkString(s, _.toString)
        new TableCell(s, s.size + " values", longText = Some(longText))
      }
      case s:String => new TableCell(s, s)
    }
  }
}

object SetSizePivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = value match {
    case s:Set[_] => new TableCell(s, s.size.toString)
    case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
    case s:String => new TableCell(s, s)
  }
}

object TradeIDPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    val tradeID = value.toString
    val label = if (tradeID.startsWith("Oil Derivatives/")) {
      val index = tradeID.lastIndexOf("/")
      tradeID.splitAt(index+1)._2
    } else {
      tradeID
    }
    TableCell(value, label, LeftTextPosition)
  }
}

object AnonPricePivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[_] if s.isEmpty => TableCell.Null
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
      case other => TableCell(other)
    }
  }
}

object AverageVolatilityPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[_] if s.isEmpty => TableCell.Null
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
      case p:Percentage => new TableCell(p, p.toShortString(formatInfo.decimalPlaces.percentageFormat, addSpace = true), RightTextPosition)
      case l:List[_] if l.size == 2 => {
        val p = l.head.asInstanceOf[Percentage]
        val s = l.tail.head.asInstanceOf[Double]
        new TableCell(l, p.toShortString(formatInfo.decimalPlaces.percentageFormat, addSpace = true) + " & " + s.format(formatInfo.decimalPlaces.defaultFormat), RightTextPosition)
      }
    }
  }
}

object EmptySetOrQuantityPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[_] if s.isEmpty => TableCell.Null
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
    }
  }
}

object DefaultPivotFormatter extends PivotFormatter {
  def limitedMkString(values:Iterable[_], format:Any=>String) = {
    val list = values.toList
    if (list.size <= 20) {
      list.map(format(_)).mkString(", ")
    } else {
      val firstTwenty = list.slice(0, 20)
      values.size + " values: " + firstTwenty.map(format(_)).mkString(", ") + " ..."
    }
  }
  def formatMany(s:Set[_], format:Any=>String) = {
    val longText = limitedMkString(s, format)
    new TableCell(s, s.size + " values", longText = Some(longText))
  }
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    try {
       value match {
         case s:Set[_] if s.isEmpty => TableCell.Null
         case s:Set[_] if s.size == 1 => new TableCell(value)
         case s:Set[_] => formatMany(s, _.toString)
         case HasLongText(text, longText) => new TableCell(text, text, longText = Some(longText))
         case v => new TableCell(v)
       }
     } catch {
       case t:Throwable => TableCell("Error during formatting:" + t.getMessage)
     }
  }
}

case class HasLongText(text:String, longText:String)

case class FieldDetails(field:Field) {
  def this(name:String) = this(Field(name))
  def name = field.name
  def nullValue():Any = "n/a"
  def nullGroup():Any = Set()
  def combineFirstGroup(value:Any):Any = combine(nullGroup(), value)
  def combine(group:Any,value:Any):Any = value match {
    case set:Set[_] => group.asInstanceOf[Set[Any]].union(set.asInstanceOf[Set[Any]])
    case _ => group.asInstanceOf[Set[Any]] + value
  }
  def combineValueOption(group:Option[Any], value:Option[Any]):Option[Any] = {
    combineOptions(group, value.map(v=>combineFirstGroup(v)))
  }
  def combineOptions(myAggregate:Option[Any], otherAggregate:Option[Any]):Option[Any] = {
    (myAggregate, otherAggregate) match {
      case (Some(agg1), Some(agg2)) => Some(combineGroup(agg1, agg2))
      case (Some(agg1), None) => Some(agg1)
      case (None, Some(agg2)) => Some(agg2)
      case (None, None) => None
    }
  }
  def combineGroup(groupA:Any,groupB:Any):Any = {
    val setA = groupA.asInstanceOf[Set[Any]]
    val setB = groupB.asInstanceOf[Set[Any]]
    if (setB.size == 1) {
      setA + setB.head
    } else if (setB.forall(setA.contains)) {
      setA
    } else {
      setA.union(setB)
    }
  }

  // NOTE - The parser returned here (and in overriding methods) must be serializable (and easily serializable at that). i.e. an Object like TextPivotParser.
  def parser:PivotParser = TextPivotParser
  def formatter:PivotFormatter = DefaultPivotFormatter
  def value(a:Any):Any = try {
    setToValue(a.asInstanceOf[Set[Any]])
  } catch {
    case e => throw new Exception("Error looking up value for " + a, e)
  }
  private def setToValue(set:Set[Any]) = {
    if (set.size == 1) {
      val value = set.toList.head
      if (value == UndefinedValue) { //If we only have n/a just show blank
        Set()
      } else {
        value
      }
    } else {
      set
    }
  }
  def matches(filterValues:Set[Any], value:Any) = filterValues.contains(value)
  def isDataField = false
  def transformValueForGroupByField(a : Any) : Any = a
  def comparator:Ordering[Any] = GenericComparator.named(field.name)
}

object GenericComparator extends Ordering[Any] {
  def compare(o1:Any, o2:Any) = {
    def sortValue(value: Any): Comparable[Any] = {
      value match {
        case s: String => s.toLowerCase.asInstanceOf[Comparable[Any]]
        case c: Comparable[_] => c.asInstanceOf[Comparable[Any]]
        case o => o.toString.toLowerCase.asInstanceOf[Comparable[Any]]
      }
    }
    
    val a = sortValue(o1)
    val b = sortValue(o2)
    try {
      val c = a.compareTo(b)
      if (c == 0 && a!=b) {
        throw new Exception( a + " and " + b + "compareTo == 0 but they are not .equal")
      }
      c
    } catch {
      case e : ClassCastException => {
        println("o1, 2 are " + o1 + ", " + o2)
        println("a, b are " + a + ", " + b)
        throw e
      }
    }
  }
}

class TreeFieldDetails(name:String) extends FieldDetails(name) {
  override def matches(filterValues: Set[Any], value: Any) = {
    val selectedPaths = filterValues.asInstanceOf[Set[PivotTreePath]]
    selectedPaths.exists(_.equalOrParentOf(value.asInstanceOf[PivotTreePath]))
  }
  override def formatter = TreePivotFormatter
}

class StrategyFieldDetails(comparator0: Ordering[PivotTreePath]) extends TreeFieldDetails("Strategy") {
  override def comparator = comparator0.asserting.untyped
}

trait Tupleable {
  def tuple: (String, String)
}

object FieldDetails {
  def apply(name:String) = new FieldDetails(name)
  def apply(name:String, parser0:PivotParser) = new FieldDetails(name) {override def parser = parser0}
  def apply(name:String, parser0:PivotParser, formatter0:PivotFormatter) = new FieldDetails(name) {
    override def parser = parser0
    override def formatter = formatter0
  }
  def createMeasure(name:String, formatter0:PivotFormatter = DefaultPivotFormatter, parser0:PivotParser = TextPivotParser) = new FieldDetails(name) {
    override def isDataField = true
    override def formatter = formatter0
    override def parser = parser0
  }
  def coded(name:String, codesToNames:Iterable[Tupleable]) = {
    val formatterParser = new CodedFormatterAndParser(codesToNames.map(_.tuple).toMap)
    new FieldDetails(name) {
      override def parser = formatterParser
      override def formatter = formatterParser
    }
  }
}

class CodedFormatterAndParser(codesToName:Map[String,String]) extends PivotFormatter with PivotParser {
  def format(value: Any, formatInfo: ExtraFormatInfo) = {
    value match {
      case values:Set[_] if values.isEmpty => TableCell.Null
      case values:Set[_] if values.size == 1 => singleValue(values.iterator.next.asInstanceOf[String])
      case values:Set[_] => DefaultPivotFormatter.formatMany(values, v => codeToName(v.asInstanceOf[String]))
      case code:String => singleValue(code)
    }
  }
  private def codeToName(code:String) = codesToName.getOrElse(code, throw new Exception("Unknown code: " + code))
  private def singleValue(code:String) = {
    val name = codeToName(code)
    new TableCell(code, name, longText = Some(name+ " [" + code+ "]"))
  }
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val lowerCaseNameToCode = codesToName.map(cn => cn._2.trim.toLowerCase -> cn._1)
    lowerCaseNameToCode.get(text.trim.toLowerCase) match {
      case Some(code) => (code, codesToName(code))
      case None => throw new Exception("Unknown value: " + text)
    }
  }
  override def acceptableValues = codesToName.values.toSet
}

case class Average(total:Double,count:Int) {
  def add(value:Double) = Average(total+value, count+1)
  def quantity = try { PivotQuantity(total/count) } catch { case e => new PivotQuantity(e) }
  def ++ (other:Average) = Average(total+other.total, count+other.count)
}

class DoubleAverageFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = Average(0,0)
  override def nullGroup = Average(0,0)
  override def combine(a:Any,b:Any) = a.asInstanceOf[Average].add(b.asInstanceOf[Double])
  override def combineGroup(a:Any,b:Any) = a.asInstanceOf[Average] ++ b.asInstanceOf[Average]
  override def value(a:Any) = a.asInstanceOf[Average].quantity
  override def formatter = StandardPivotQuantityFormatter
  override def isDataField = true
}

case class PivotQuantityAverage(total:PivotQuantity,count:Map[UOM,Int]) {
  def this(q:PivotQuantity) = this(q, q.values.mapValues(v => 1))
  def add(value:PivotQuantity) = this ++ new PivotQuantityAverage(value)
  def quantity = PivotQuantity(total.values.map { case(uom,value) => uom->value/count(uom) }, total.errors)
  def ++ (other:PivotQuantityAverage) = {
    val onlyInThat = other.count -- count.keySet
    PivotQuantityAverage(total + other.total, onlyInThat ++ (for((uom,value) <- count) yield uom->(other.count.getOrElse(uom, 0) + value)))
  }
}

class AveragePivotQuantityFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = PivotQuantityAverage(PivotQuantity.NULL,Map())
  override def nullGroup = PivotQuantityAverage(PivotQuantity.NULL,Map())
  override def combine(a:Any,b:Any) = a.asInstanceOf[PivotQuantityAverage].add(b.asInstanceOf[PivotQuantity])
  override def combineGroup(a:Any,b:Any) = a.asInstanceOf[PivotQuantityAverage] ++ b.asInstanceOf[PivotQuantityAverage]
  override def value(a:Any) = a.asInstanceOf[PivotQuantityAverage].quantity
  override def formatter = StandardPivotQuantityFormatter
  override def isDataField = true
  override def comparator = PivotQuantityComparator
}

class PercentageLabelFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def value(a:Any):Any = setToValue(a.asInstanceOf[Set[Any]])
  override def formatter = PercentagePivotFormatter
  override def parser = PercentagePivotParser
  private def setToValue(set:Set[Any]) = {
    if (set.size == 1) {
      set.toList.head
    } else {
      set
    }
  }
}

object PercentagePivotParser extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val textNoCommas = text.replaceAll(",", "").trim()
    val cleanTextMaybeWithPercentage = if (textNoCommas.startsWith("(")) {
      "-" + textNoCommas.replaceAll("\\(", "").replaceAll("\\)", "")
    } else {
      textNoCommas
    }
    val doubleValue = if (cleanTextMaybeWithPercentage.contains("%")) {
      val textWithoutPercentage = cleanTextMaybeWithPercentage.replace("%", "")
      textWithoutPercentage.toDouble / 100.0
    } else {
      cleanTextMaybeWithPercentage.toDouble
    }
    val percentage = Percentage(doubleValue)
    (percentage, PercentagePivotFormatter.format(percentage, extraFormatInfo).text)
  }
}

object PercentagePivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[_] if s.size > PivotFormatter.MaxSetSize => new TableCell(s, s.size + " values")
      case s:Set[_] => new TableCell(s, s.map(_.asInstanceOf[Percentage].toShortString(formatInfo.decimalPlaces.percentageFormat, addSpace = true)).mkString(","))
      case p:Percentage => new TableCell(p, p.toShortString(formatInfo.decimalPlaces.percentageFormat, addSpace = true), RightTextPosition, longText = Some(p.toLongString))
    }
  }
}

class SumPivotQuantityFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = PivotQuantity.NULL
  override def nullGroup = PivotQuantity.NULL
  override def combine(a:Any,b:Any) = (a, b) match {
    case (q1: PivotQuantity, q2: PivotQuantity) => q1 + q2
    case _ => throw new Exception("Unexpected types: " + (a, b))
  }
  override def parser = PivotQuantityPivotParser
  override def combineGroup(a:Any,b:Any) = a.asInstanceOf[PivotQuantity] + b.asInstanceOf[PivotQuantity]
  override def value(a:Any) = a
  override def formatter:PivotFormatter = StandardPivotQuantityFormatter
  override def isDataField = true
  override def comparator = PivotQuantityComparator
}

object StandardPivotQuantityFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
      case q:Quantity => QuantityLabelPivotFormatter.format(q, formatInfo)
    }
  }
}

/**
 * Used to display 'Quantity', i.e. the volume of a UTP or Tradeable
 */
class TradeIDGroupingSumPivotQuantityFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = Map[String, Quantity]()
  override def nullGroup = Map[String, Quantity]()
  override def combine(a:Any,b:Any) = {
    a.asInstanceOf[Map[String, Quantity]] ++ b.asInstanceOf[Map[String, Quantity]]
  }
  override def combineGroup(a:Any,b:Any) = {
    a.asInstanceOf[Map[String, Quantity]] ++ b.asInstanceOf[Map[String, Quantity]]
  }
  private def asPivotQuantity(a : Any) : PivotQuantity = {
    a.asInstanceOf[Map[String, Quantity]].valuesIterator.toList.map(PivotQuantity(_)).sum
  }
  override def value(a:Any) = asPivotQuantity(a)
  override def formatter = StandardPivotQuantityFormatter
  override def isDataField = true

  override def transformValueForGroupByField(a: Any) = {
    a match {
      case q:Quantity => q
      case map:Map[_,_] => {
        assert(map.size == 1, "Map should only have one value: " + map)
        map.valuesIterator.next
      }
      case UndefinedValue => UndefinedValue
      case o => throw new IllegalArgumentException("Don't know how to handle something that isn't a Quantity or Map : " + o.asInstanceOf[AnyRef].getClass.getName)
    }
  }
  override def comparator = QuantityComparator
}

class MarketValueFieldDetails(name: String) extends FieldDetails(Field(name)) {
  override def value(a:Any):Any = a.asInstanceOf[Set[Any]]
  override def formatter = MarketValueSetPivotFormatter
  override def isDataField = true
  override def comparator = new MarketValueComparer(super.comparator)
  override def parser = MarketValuePivotParser
}

object MarketValueSetPivotFormatter extends PivotFormatter {
  def format(value: Any, formatInfo: ExtraFormatInfo) = value match {
    case s:Set[_] => s.toList match {
      case List(pq: PivotQuantity) => TableCell.fromPivotQuantity(pq, formatInfo)
      case List(p: Percentage) => PercentagePivotFormatter.format(p, formatInfo)
      case list:List[_] => new TableCell(list.size + " values")
    }
    case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
  }
}

class MarketValueComparer(backup: Ordering[Any]) extends Ordering[Any] {
  def compare(x: Any, y: Any) = (x, y) match {
    case (left: PivotQuantity, right:PivotQuantity) => PivotQuantityComparator.compare(left, right)
    case _ => backup.compare(x, y)
  }
}

object MarketValuePivotParser extends PivotParser {
  def parse(text: String, extraFormatInfo:ExtraFormatInfo) = (MarketValue.fromString(text).pivotValue, text)
}

class PivotQuantityFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def value(a:Any):Any = a.asInstanceOf[Set[Any]]
  override def formatter = PivotQuantitySetPivotFormatter
  override def parser =  PivotQuantityPivotParser
  override def nullValue = PivotQuantity.NULL
  override def isDataField = true
  override def comparator = PivotQuantityComparator
}

object PivotQuantitySetPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[PivotQuantity] if s.size == 0 => TableCell.Null
      case s:Set[PivotQuantity] if s.size == 1 => TableCell.fromPivotQuantity(s.iterator.next, formatInfo)
      case s:Set[PivotQuantity] => new TableCell(s, s.size + " values", longText = Some(s.map(TableCell.longText).flatten.mkString(", ")))
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
    }
  }
}

class PivotSpreadQuantityFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def value(a:Any):Any = setToValue(a.asInstanceOf[Set[Any]])
  override def formatter = PivotSpreadQuantityPivotFormatter
  override def nullValue = SpreadOrQuantity(Left(Quantity.NULL))
  private def setToValue(set:Set[Any]) = {
    if (set.size == 1) {
      set.toList.head match {
        case UndefinedValue => Set()
        case q: Quantity => PivotQuantity(q)
        case s: SpreadQuantity => s
        case sq: SpreadOrQuantity => sq.either match {
          case Left(q) => PivotQuantity(q)
          case Right(s) => s
        }
      }
    } else {
      set
    }
  }
  override def comparator = SpreadOrQuantityComparator
}

object PivotSpreadQuantityPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    def spreadQuantityToTableCell(sq:SpreadQuantity) = {
      val front = sq.front
      val back = sq.back
      val label = front.value.format(formatInfo.decimalPlaces.format(front.uom)) + "/" +
              back.value.format(formatInfo.decimalPlaces.format(back.uom)) + front.uom
      new TableCell(sq, label, RightTextPosition)
    }
    value match {
      case s:Set[_] if s.isEmpty => TableCell.Null
      case sq:SpreadQuantity => spreadQuantityToTableCell(sq)
      case soq:SpreadOrQuantity => {
        soq.either match {
          case Left(q) => {
            val label = q.value.format(formatInfo.decimalPlaces.format(q.uom)) + q.uom.toString
            new TableCell(q, label, RightTextPosition)
          }
          case Right(sq) => spreadQuantityToTableCell(sq)
        }
      }
      case pq:PivotQuantity => TableCell.fromPivotQuantity(pq, formatInfo)
      case s:Set[_] if s.size > PivotFormatter.MaxSetSize => new TableCell(s, s.size + " values")
      case s:Set[_] => new TableCell(s, s.mkString(","))
    }
  }
}

class QuantityLabelFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def value(a:Any):Any = setToValue(a.asInstanceOf[Set[Any]])
  override def formatter = QuantityLabelPivotFormatter
  override def nullValue() = Quantity.NULL
  private def setToValue(set:Set[Any]) = {
    if (set.size == 1) {
      set.toList.head
    } else {
      set
    }
  }
  override def comparator = QuantityComparator
}

object QuantityLabelPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    value match {
      case s:Set[_] if s.size > PivotFormatter.MaxSetSize => new TableCell(s, s.size + " values")
      case s:Set[_] => new TableCell(s, s.mkString(","))
      case other => {
        val label = other match {
          case q:Quantity => q.value.format(formatInfo.decimalPlaces.format(q.uom)) + q.uom
          case _ => other.toString
        }
        new TableCell(other, label, RightTextPosition)
      }
    }
  }
}

class SumIntFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = 0
  override def nullGroup = 0
  override def combine(a:Any,b:Any) = {
    if (! (a.isInstanceOf[Int] && b.isInstanceOf[Int])){
      throw new IllegalStateException("Field " + name + " a = " + a + ", " + a.asInstanceOf[AnyRef].getClass + ", b = " + b + ", " + b.asInstanceOf[AnyRef].getClass)
    }
    a.asInstanceOf[Int] + b.asInstanceOf[Int]
  }
  override def combineGroup(a:Any,b:Any) = a.asInstanceOf[Int] + b.asInstanceOf[Int]
  override def value(a:Any) = a
  override def formatter = ToStringPivotFormatter
  override def isDataField = true
  override def parser = IntPivotParser
}

object ToStringPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = new TableCell(value, value.toString)
}

class SumDoubleFieldDetails(name:String) extends FieldDetails(Field(name)) {
  override def nullValue = 0d
  override def nullGroup = 0d
  override def combine(a:Any,b:Any) = a.asInstanceOf[Double] + b.asInstanceOf[Double]
  override def combineGroup(a:Any,b:Any) = a.asInstanceOf[Double] + b.asInstanceOf[Double]
  override def value(a:Any) = a.asInstanceOf[Double]
  override def formatter = DoublePivotFormatter
  override def isDataField = true
}

object DoublePivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = new TableCell(value, value.asInstanceOf[Double].format(formatInfo.decimalPlaces.defaultFormat))
}