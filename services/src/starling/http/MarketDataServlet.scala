package starling.http

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.gui.api.{SpecificMarketDataVersion, MarketDataIdentifier, MarketDataSelection, PricingGroup}
import starling.db.MarketDataStore
import starling.marketdata.MarketDataTypes
import starling.utils.Utils
import starling.pivot.model.PivotTableModel
import collection.immutable.TreeMap
import starling.utils.ImplicitConversions._
import starling.daterange.Day
import scala.collection.JavaConversions
import starling.pivot.{SomeSelection, PivotFieldParams, PivotFieldsState, Field}

class MarketDataServlet(marketDataStore:MarketDataStore) extends HttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = {
    val p = req.getParameterMap
    val params = HttpParams(req)

    val pricingGroup = params.get("pricingGroup").map(name => PricingGroup.fromName(name(0)))
    val excel = params.get("excel").map(_.mkString(","))
    val selection = MarketDataSelection(pricingGroup, excel)

    params.getFirst("version") match {
      case None => {
        val latest = marketDataStore.latestMarketDataIdentifier(selection).
          marketDataVersion.asInstanceOf[SpecificMarketDataVersion].version
        val redirect: String = params.replace("version", latest.toString).toUrl
        resp.sendRedirect("?" + redirect)
      }
      case Some(version) => {
        val mdi = MarketDataIdentifier(selection, new SpecificMarketDataVersion(version.toInt))

        val marketDataType = MarketDataTypes.fromName(req.getParameter("type"))

        val pivot = marketDataStore.pivot(mdi, marketDataType)

        def fieldList(param:String) = params.split(param).map(f => Field(f.replaceAll("_", " ")))
        val data = fieldList("measures")
        val rows = fieldList("rows")
        val columns = fieldList("columns")

        val filters = params.filterKeys(_.startsWith("@")).map { case(f, vs) => {
          val field = Field(f.substring(1).replaceAll("_", " "))
          val parser = pivot.lookup(field).parser

          field → SomeSelection(vs.flatMap(_.split(":")).toList.map(v => parser.parse(v)._1).toSet)
        } }.toList

        val pfs = PivotFieldsState.apply(
          dataFields = data,
          rowFields = rows,
          columnFields = columns,
          filters = filters
        )

        //TODO
        //redirect to latest if no version specified
        //support any filter
        //PricingGroup and Excel Drop Downs
        //Filter drop downs
        //Drag and drop fields

        val pivotData = PivotTableModel.createPivotData(pivot, PivotFieldParams(true, Some(pfs)))

        val (contentType, content) = req.getHeader("Accept") ?? "text/plain" match {
          case accept if accept.containsOneOf("text/html", "*/*") =>
            "text/html" → pivotData.pivotTable.convertUsing(Utils.tableConverter)
          case accept => "text/plain" → pivotData.pivotTable.asCSV
        }
        resp.setContentType(contentType)
        resp.getWriter.write(content)
      }
    }
  }
}

case class HttpParams(params: Map[String, Array[String]]) {
  def get(name: String) = params.get(name).map(_.toList)
  def getFirstOrElse(name: String, default: String) = getFirst(name).getOrElse(default)
  def getFirst(name: String) = get(name).map(_.headOption).flatOpt
  def split(name: String, sep: String = ":") = get(name).getOrElse(Nil).flatMap(_.split(sep))

  def filterKeys(pred: String => Boolean) = params.filterKeys(pred)

  def replace(name: String, value: String) = copy(params = params + (name → Array(value)))
  def toUrl() = params.toList.flatMap{ case(p,vs) => vs.map(v => p+"="+v)}.mkString("&")
}

object HttpParams {
  def apply(request: HttpServletRequest) =
    new HttpParams(JavaConversions.asScalaMap(request.getParameterMap).toMap.asInstanceOf[Map[String, Array[String]]])
}

