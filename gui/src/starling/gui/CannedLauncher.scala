package starling.gui

import api._
import java.lang.String
import pages.PivotTablePageData._
import pages.{PivotTablePageData, AbstractPivotPage, SingleTradePage, PivotPageState}
import swing.event.ButtonClicked
import starling.pivot.model._
import java.awt.Dimension
import starling.pivot._
import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import starling.rmi.StarlingServer
import collection.Seq
import starling.quantity.{UOM, Quantity}
import starling.auth.User
import starling.daterange.{Month, Year, Day}
import collection.immutable.{Map, Iterable}
import starling.browser._
import common.{NumberedButton, MigPanel}
import service._
import starling.browser.internal.UserSettings
import service.internal.HeterogeneousMap
import xstream.GuiStarlingXStream
import swing.{Component, Label, Button}

/**
 * An alternative StarlingBrowser for testing gui features quickly
 */

object CannedLauncher {
  def main(args:Array[String]) {
    System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
    BrowserLauncher.start(new scala.swing.Publisher() {}, new HeterogeneousMap[LocalCacheKey], None) {
      new ServerContext {
        def lookup[T](klass: Class[T]) = throw new Exception("Canned launcher has no services")
        def version = Version("canned", "hostname", "db", false, None)
        def browserBundles = List(CannedBrowserBundle)
        def browserService = CannedBrowserService
        def username = "Canned User"
      }
    }
  }
}

object CannedBrowserService extends BrowserService {
  def name = "Canned"
  def readSettings = UserSettingsLabel(Nil)
  def saveSettings(settings: UserSettingsLabel) {println("We don't save canned settings")}
  def saveBookmark(bookmark: BookmarkLabel) {println("We don't save canned bookmarks")}
  def deleteBookmark(name: String) {println("We don't delete canned bookmarks")}
  def bookmarks = Nil
  def logPageView(info: PageLogInfo) { /*skip*/ }
}

object CannedBrowserBundle extends BrowserBundle {
  def bundleName = "Canned"
  def marshal(obj: AnyRef) = GuiStarlingXStream.write(obj)
  def unmarshal(text: String) = GuiStarlingXStream.read(text).asInstanceOf[AnyRef]
  override def homeButtons(pageContext: PageContext) = CannedHomePagePageComponent.buttons
}

class NullPageData extends PageData

class CannedServerContext
trait CannedPage extends Page {
  def bundle = "Canned"
  type SC = CannedServerContext
  def createServerContext(sc:ServerContext) = new CannedServerContext
}

case class CannedHomePage() extends CannedPage {
  def text = "Home"
  def build(reader: CannedServerContext) = { new PageData{} }
  def createComponent(context: PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = {
    new CannedHomePagePageComponent(context)
  }
  def icon = StarlingIcons.im("/icons/tablenew_16x16.png")
}

object CannedHomePagePageComponent {
  val icon = StarlingIcons.im("/icons/stock_chart-reorganize.png")
  def buttons = {
    PageButton("Run", CannedPivotReportPage(PivotPageState(false, PivotFieldParams(true, None))), icon) ::
    PageButton("Run Editable", EditableCannedPivotReportPage(PivotPageState(false, PivotFieldParams(true, None))), icon) ::
    PageButton("Run Editable Specified", EditableSpecifiedCannedPivotReportPage(PivotPageState(false, PivotFieldParams(true, None))), icon) ::
    PageButton("Run Slow", SlowCannedPivotReportPage(PivotPageState(false, PivotFieldParams(true, None))), icon) ::
    Nil
  }
}
class CannedHomePagePageComponent(pageContext:PageContext) extends MigPanel("") with PageComponent {

  var firstButton:Component = null
  CannedHomePagePageComponent.buttons.foreach { button =>
    val buttonComponent = new NumberedButton(button.name, button.icon, (modifiers) => {
            pageContext.createAndGoTo( (serverContext) => button.pageFactory.create(serverContext), modifiers = modifiers) })
    if (firstButton == null) firstButton = buttonComponent
    add(buttonComponent)
  }

  override def defaultComponentForFocus = Some(firstButton.peer)
}

case class CannedDrilldownPage(fields:Seq[(Field,Any)]) extends CannedPage {
  def text = "Canned drilldown"
  def build(reader: CannedServerContext) = new PageData() {}
  def createComponent(context: PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = {
    new MigPanel("") with PageComponent {
      add(new Label("Drilldown"), "span")
      for ((field,value) <- fields) {
        add(new Label("   " + field.name + " => " + value), "span")
      }
    }
  }
  def icon = StarlingIcons.im("/icons/tablenew_16x16.png")
}

abstract class AbstractCannedPivotPage(pivotPageState:PivotPageState, edits:PivotEdits = PivotEdits.Null) extends AbstractPivotPage(pivotPageState) with CannedPage {

}

case class SlowCannedPivotReportPage(pivotPageState:PivotPageState) extends AbstractCannedPivotPage(pivotPageState) {
  override def text = "Slow Canned Pivot Report"
  def dataRequest(pageBuildingContext:CannedServerContext) = {
    Thread.sleep(5*1000);
    PivotTableModel.createPivotData(new CannedDataSource, pivotPageState.pivotFieldParams)
  }
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = SlowCannedPivotReportPage(pivotPageState)
  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers) {pageContext.goTo(CannedDrilldownPage(fields), modifiers)}
}

case class DiffCannedPivotReportPage(pivotPageState:PivotPageState) extends AbstractCannedPivotPage(pivotPageState) {
  override def text = "Diff Canned Pivot Report"
  def dataRequest(pageBuildingContext:CannedServerContext) = {
    val cannedDataSource = new CannedDataSource
    PivotTableModel.createPivotData(new DiffPivotTableDataSource(cannedDataSource, cannedDataSource, "D-1"), pivotPageState.pivotFieldParams)
  }
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = DiffCannedPivotReportPage(pivotPageState)
}

case class CannedPivotReportPage(pivotPageState:PivotPageState) extends AbstractCannedPivotPage(pivotPageState) {
  override def text = "Canned Pivot Report"
  def dataRequest(pageBuildingContext:CannedServerContext) = {
    PivotTableModel.createPivotData(new CannedDataSource, pivotPageState.pivotFieldParams)
  }
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = CannedPivotReportPage(pivotPageState)
  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers) {pageContext.goTo(CannedDrilldownPage(fields), modifiers)}
}

case class EditableCannedPivotReportPage(pivotPageState:PivotPageState) extends AbstractCannedPivotPage(pivotPageState) {
  override def text = "Editable Canned Pivot Report"
  def dataRequest(pageBuildingContext:CannedServerContext) = {
    PivotTableModel.createPivotData(new EditableCannedDataSource, pivotPageState.pivotFieldParams)
  }
  def selfPage(pPS:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pPS)
  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers) {pageContext.goTo(CannedDrilldownPage(fields), modifiers)}
}

case class EditableSpecifiedCannedPivotReportPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null) extends AbstractCannedPivotPage(pivotPageState, edits) {
  override def text = "Editable Canned Pivot Report With Specified Values"
  def dataRequest(pageBuildingContext:CannedServerContext) = {
    val ds = (new EditableSpecifiedCannedDataSource).editable.get.withEdits(edits)
    PivotTableModel.createPivotData(ds, pivotPageState.pivotFieldParams)
  }
  def selfPage(pPS:PivotPageState, edits0:PivotEdits) = copy(pivotPageState = pPS, edits = edits0)
  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers) {pageContext.goTo(CannedDrilldownPage(fields), modifiers)}

  override def save(starlingServer:ServerContext, edits:PivotEdits) = {
    println("EditableSpecifiedCannedPivotReportPage saved these " + " edits " + edits)
    true
  }
}

object CannedDeltaPivotFormatter extends PivotFormatter {
  def format(value:Any, formatInfo:ExtraFormatInfo) = {
    StandardPivotQuantityFormatter.format(value, formatInfo).copy(longText = Some("Delta explanation"))
  }
}

/**
 * A data source used for testing the scaling of the pivot table.
 */
class CannedDataSource extends UnfilteredPivotTableDataSource {
  private val num = 1000

  private val traders = List("corin", "brian", "kieth","alex","mike", "Iamatraderwithareallylongnameitisreallyverylongohyesitis")
//  private val traderBookLookup = List("alex")
  private val products = List("BRENT", "WTI", "COAL","GAS","PAPER","ABC")
  private val strikes = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160)
  private val expiryMonths = Year(2009).toListOfMonths
  private val lots = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, 500, 1000, 5000, 10000, 50000, 100000)
  val localFields : List[FieldDetails] = new SumPivotQuantityFieldDetails("PV") {
    override def isDataField = true
    override def parser = PivotQuantityPivotParser
  } :: new SumPivotQuantityFieldDetails("Delta") {
    override def isDataField = true
    override def formatter:PivotFormatter = CannedDeltaPivotFormatter
  } :: new SumPivotQuantityFieldDetails("Gamma") {
    override def isDataField = true
  } :: List("Trade", "Trader", "Product", "Strike", "Expiry", "Lots").map{ f => new FieldDetails(f)}

  val random = new java.util.Random(1234567890L)
  private val theData : List[Map[Field, Any]] = {
    val l = for(i <- 0 until num) yield {
      val trade = "T"+i
      val trader = traders(random.nextInt(traders.size))
      val product = products(random.nextInt(products.size))
      val strike = strikes(random.nextInt(strikes.size))
      val expiry = expiryMonths(random.nextInt(expiryMonths.size))
      val lot = lots(random.nextInt(lots.size))
      val pv = Quantity(random.nextGaussian * 100.0, UOM.USD)
      val gamma = random.nextGaussian * 10.0
      Map(localFields(3).field -> trade,localFields(4).field -> trader, localFields(5).field -> product,
        localFields(6).field -> strike, localFields(7).field -> expiry, localFields(8).field -> lot,
        localFields(0).field -> PivotQuantity(pv), localFields(2).field -> PivotQuantity(gamma))
    }
    l.toList
  } ::: {
    (for(i <- 0 until num) yield {
      val trade = "T"+i
      val trader = traders(random.nextInt(traders.size))
      val pv = Quantity(random.nextGaussian * 100.0, UOM.USD)
      val gamma = random.nextGaussian * 10.0
      val delta = random.nextGaussian * 100.0
      Map(localFields(3).field -> trade,localFields(4).field -> trader,
        localFields(0).field -> PivotQuantity(pv), localFields(1).field -> PivotQuantity(delta),
        localFields(2).field -> PivotQuantity(gamma))
    }).toList
  }

  override def drillDownGroups = List(
    DrillDownInfo(PivotAxis(List(), List(Field("Strike")),List(), false)),
    DrillDownInfo(PivotAxis(List(), List(Field("Expiry")),List(), false)),
    DrillDownInfo(PivotAxis(List(), List(Field("Trade")),List(), false)))

  /*override def initialState = new PivotFieldsState(columns = {
    ColumnTrees(List(
      ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true)))
  }, rowFields = List(Field("Trader"), Field("Strike")))*/
  override def initialState = new PivotFieldsState(columns = {
    ColumnTrees(List(
      ColumnTree(Field("PV"), true), ColumnTree(Field("Gamma"), true)))
  }, rowFields = List(Field("Trader")))

  def unfilteredData(pfs : PivotFieldsState) = theData

  val fieldDetailsGroups = {
    val (group1Fields, group2Fields) = localFields.splitAt(localFields.size / 2)
    List(FieldDetailsGroup("Group 1", group1Fields), FieldDetailsGroup("Group 2", group2Fields))
  }
  override def zeroFields = Set(Field("Delta"))
  override def toString = "BigCannedDataSource"
}

class EditableCannedDataSource extends CannedDataSource {
  override def editable = Some(new EditPivot {
    def save(edits:PivotEdits) = {println("SAVE : " + edits); true}
    def editableToKeyFields = Map(Field("PV") -> Set(Field("Lots")), Field("Gamma") -> Set(Field("Lots"), Field("Product"), Field("Strike")))
    def withEdits(edits:PivotEdits) = null
  })

}

class EditableSpecifiedCannedDataSource extends UnfilteredPivotTableDataSource {
  private val traders = List("corin", "brian", "kieth","alex","mike", "Iamatraderwithareallylongnameitisreallyverylongohyesitis")
//  private val months = List(Month(2011, 1),Month(2011, 2),Month(2011, 3))
  private val markets = List("BRENT", "WTI", "COAL","GAS","PAPER","ABC", "abe", "What", "Which", "when")

  def data:List[Map[Field, Any]] = {
    val random = new java.util.Random(1234567890L)
    (for (trader <- traders; market <- markets) yield {
      if (random.nextInt(9) > 3) {
        Some(Map(Field("Trader") -> trader, Field("Market") -> market, Field("Volume") -> random.nextInt(2000), Field("Single Value") -> "Single Value Nick", Field("Non Editable") -> "bla"))
      } else {
        None
      }
    }).flatten ::: List(Map(Field("Trader") -> "alex", Field("Market") -> "Unused", Field("Volume") -> 15, Field("Single Value") -> "Single Value Nick"))
  }

  val marketFieldDetails = new FieldDetails("Market") {
    override def parser = new CannedMarketPivotParser(markets.toSet + "Unused")
  }

  def fieldDetailsGroups = List(FieldDetailsGroup("Group 1", FieldDetails("Trader"), marketFieldDetails, new SumIntFieldDetails("Volume"), FieldDetails("Single Value"), FieldDetails("Non Editable")))
  def unfilteredData(pfs:PivotFieldsState) = data

  override def editable = Some(new EditPivot {
    def save(edits:PivotEdits) = {println("SAVE : " + edits); true}
    def editableToKeyFields = Map(Field("Volume") -> Set(Field("Trader"), Field("Market"), Field("Single Value")))
    def withEdits(edits:PivotEdits):PivotTableDataSource = {
      if (edits.isEmpty) {
        EditableSpecifiedCannedDataSource.this
      } else {
        val d:List[Map[Field, Any]] = EditableSpecifiedCannedDataSource.this.data
        new EditableSpecifiedCannedDataSource {
          override def data = {
            println("")
            println("!!! Edits : " + (edits.edits.size, edits))
            println("")

            val dWithDeletesAndAmends = d.map(m => {
              val key = Map(Field("Trader") -> m(Field("Trader")), Field("Market") -> m(Field("Market")), Field("Single Value") -> m(Field("Single Value")))
              m.map { case (field, value) => {
                edits.editFor(key, field) match {
                  case None => field -> value
                  case Some((matchedKey, edit)) => field -> edit.applyEdit(matchedKey, field, value)
                }
              }
            }})

            val addedRows = edits.newRows.zipWithIndex.map{case (row,index) => {
              Map() ++ fieldDetailsMap.keySet.map(f => {
                f -> NewValue(row.get(f), index, PivotEdits.Null.withAddedRow(row))
              })
            }}.toList

            dWithDeletesAndAmends ::: addedRows
          }
        }
      }
    }
  })

  override def initialState = new PivotFieldsState(rowFields = List(Field("Trader"), Field("Market")), columns = ColumnTrees(Field("Volume"), true))
}

class CannedMarketPivotParser(markets:Set[String]) extends PivotParser {
  def parse(text:String, extraFormatInfo:ExtraFormatInfo) = {
    val lowerCaseMarkets = markets.map(_.trim().toLowerCase)
    if (lowerCaseMarkets(text.trim.toLowerCase)) {
      (text, text)
    } else {
      throw new Exception("Unknown Market")
    }
  }
  override def acceptableValues = markets
}