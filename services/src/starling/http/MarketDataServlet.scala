package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import scala.collection.JavaConversions

import starling.db.MarketDataStore
import starling.gui.api.{MarketDataIdentifier, MarketDataSelection, PricingGroup}
import starling.marketdata.MarketDataTypes
import starling.pivot.model.PivotTableModel
import starling.utils.Utils

import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.utils.Pattern._


class MarketDataServlet(marketDataStore:MarketDataStore) extends HttpServlet {
  private val FilterField = Extractor.when[String](_.startsWith("@"), _.stripPrefix("@").replaceAll("_", " "))
  private val ConcatenatedFields = Extractor.map[Array[String]](_.flatMap(_.split(":")).toList)
  private val decimalPlaces = PivotFormatter.DefaultDecimalPlaces.copy(percentageFormat = "#0.0000")

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {
    val params = HttpParams(request)
    def fieldList(param: String) = params.split(param, ":").map(f => Field(f.replaceAll("_", " ")))

    val selection = MarketDataSelection(PricingGroup.fromName(params.getFirst("pricingGroup")), params.getFirst("excel"))

    params.getFirst("version") match {
      case None => response.sendRedirect("?" + params.replace("version", marketDataStore.latest(selection)).toUrl)
      case Some(version) => {
        val mdi = MarketDataIdentifier(selection, version.toInt)

        val marketDataType = MarketDataTypes.fromName(request.getParameter("type"))

        val pivot = marketDataStore.pivot(mdi, marketDataType)

        val data = fieldList("measures")
        val rows = fieldList("rows")
        val columns = fieldList("columns")

        val filters = params.params.collect { case (FilterField(field), ConcatenatedFields(vs)) =>
          Field(field) → SomeSelection(vs.map(v => pivot.lookup(field).parser.parse(v)._1).toSet)
        }

        val pfs = PivotFieldsState.apply(
          dataFields = data,
          rowFields = rows,
          columnFields = columns,
          filters = filters.toList
        )

        //TODO [06 Jun 2011] support any filter
        //TODO [06 Jun 2011] PricingGroup and Excel Drop Downs
        //TODO [06 Jun 2011] Filter drop downs
        //TODO [06 Jun 2011] Drag and drop fields

        val pivotData = PivotTableModel.createPivotData(pivot, PivotFieldParams(true, Some(pfs)))

        val (contentType, content) = request.getHeader("Accept") ?? "text/plain" match {
          case accept if accept.containsOneOf("text/html", "*/*") =>
            "text/html" → pivotData.pivotTable.convertUsing(Utils.tableConverter, decimalPlaces)
          case accept => "text/plain" → pivotData.pivotTable.asCSV
        }

        response.setContentType(contentType)
        response.getWriter.write(content)
      }
    }
  }
}

case class HttpParams(params: Map[String, Array[String]]) {
  def get(name: String) = params.get(name).map(_.toList)
  def getFirstOrElse(name: String, default: => String) = getFirst(name).getOrElse(default)
  def getFirst(name: String) = get(name).map(_.headOption).flatOpt
  def split(name: String, sep: String) = get(name).getOrElse(Nil).flatMap(_.split(sep))

  def filterKeys(pred: String => Boolean) = params.filterKeys(pred)

  def replace(name: String, value: Any) = copy(params = params + (name → Array(value.toString)))
  def toUrl() = params.toList.flatMap{ case(p,vs) => vs.map(v => p+"="+v)}.mkString("&")
}

object HttpParams {
  def apply(request: HttpServletRequest) =
    new HttpParams(JavaConversions.mapAsScalaMap(request.getParameterMap).toMap.asInstanceOf[Map[String, Array[String]]])
}
