package starling.gui.pages

import starling.browser.internal.RootBrowserBundle
import java.awt.Dimension
import starling.browser._
import common.MigPanel
import swing._
import collection.mutable.{Stack => MStack}
import event.Event
import starling.bouncyrmi.{MethodLogService, MethodLogEvent}
import starling.pivot.model.PivotTableModel
import starling.pivot.FieldDetails._
import starling.pivot.FieldDetailsGroup._
import starling.pivot.PivotFieldsState._
import starling.pivot._
import starling.gui.{StarlingLocalCache, StarlingIcons}
import StarlingLocalCache._
import starling.quantity.{UOM, Quantity}

case class MethodLogViewerPage(index:Int, pivotPageState:PivotPageState=PivotPageState()) extends AbstractPivotPage(pivotPageState) {
  def text = "Method Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_event.png")
  def bundle = RootBrowserBundle.bundleName
  override def latestPage(localCache: LocalCache) = copy(index=localCache.methodLogIndex)

  def dataRequest(serverContext: ServerContext) = {
    val pivot = new UnfilteredPivotTableDataSource() {
      val id               = FieldDetails("ID")
      val service          = FieldDetails("Service")
      val method           = FieldDetails("Method")
      val compressedSize   = new SumPivotQuantityFieldDetails("Compressed Size")
      val uncompressedSize = new SumPivotQuantityFieldDetails("Uncompressed Size")
      val rate             = new SumPivotQuantityFieldDetails("Rate")
      val serverTime       = new SumPivotQuantityFieldDetails("Server Time")
      val ioTime           = new SumPivotQuantityFieldDetails("IO Time")
      val totalTime        = new SumPivotQuantityFieldDetails("Invocation Time")

      val rowFields = List(id, service, method)
      val dataFields = List(compressedSize, uncompressedSize, rate, serverTime, ioTime, totalTime)
      def fieldDetailsGroups = List(FieldDetailsGroup("Method Viewer", rowFields ::: dataFields))

      override val initialState = PivotFieldsState(rowFields = fields(rowFields), dataFields = fields(dataFields))

      private def pq(value:Long, uom:UOM) = new Quantity(value, uom).pq

      def unfilteredData(pfs : PivotFieldsState) = {
        serverContext.lookup(classOf[MethodLogService]).methodInvocations(index).map { entry =>
          fields(
            id → entry.id,
            service → entry.serviceName,
            method → entry.method.getName,
            uncompressedSize → pq(entry.uncompressedSize, UOM.BYTES),
            compressedSize → pq(entry.compressedSize, UOM.BYTES),
            rate -> Quantity(entry.rate, UOM.BYTES / UOM.MILLISECONDS).pq,
            totalTime → pq(entry.time, UOM.MILLISECONDS),
            ioTime → pq(entry.ioTime, UOM.MILLISECONDS),
            serverTime → pq(entry.serverTime, UOM.MILLISECONDS)
          )
        }
      }
    }
    PivotTableModel.createPivotData(pivot, pivotPageState.pivotFieldParams)
  }
  def selfPage(pivotPageState: PivotPageState, edits: PivotEdits) = copy(pivotPageState=pivotPageState)

  type SC = ServerContext
  def createServerContext(sc: ServerContext) = sc
}

class MethodLogPageData(val events:List[MethodLogEvent]) extends PageData
