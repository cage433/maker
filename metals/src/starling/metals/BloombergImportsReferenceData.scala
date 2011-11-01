package starling.metals

import datasources.BloombergImports
import starling.pivot._
import scalaz.Scalaz._
import starling.daterange.TimeZone


class BloombergImportsReferenceData(bloombergImports: BloombergImports) extends UnfilteredPivotTableDataSource {
  val lim@List(limFolder, limColumn, limSymbol, limDescription, expectedTimeGMT, expectedTime, exportToLim) =
    fieldDetails("Folder", "Column", "Lim Symbol", "Description", "Expected Time (GMT)", "Expected Time", "Export to LIM")
  val bloomberg@List(quoteId, symbol) = fieldDetails("Quote Id", "Symbol")

  def fieldDetailsGroups = List(FieldDetailsGroup("Lim", lim), FieldDetailsGroup("Bloomberg", bloomberg))
  override val initialState = PivotFieldsState(rowFields = fields(quoteId), dataFields = fields(symbol :: lim))

  def unfilteredData(pfs: PivotFieldsState) = bloombergImports.imports.map { bloombergImport => fields(
    quoteId         → bloombergImport.quoteId,
    symbol          → (bloombergImport.symbol | ""),
    limSymbol       → (bloombergImport.limSymbol | ""),
    limFolder       → (bloombergImport.limFolder | ""),
    limColumn       → (bloombergImport.limColumn | ""),
    limDescription  → (bloombergImport.limDescription | ""),
    expectedTimeGMT → (bloombergImport.expectedTime(TimeZone.GMT).toString("HH:mm") + " GMT"),
    expectedTime    → (bloombergImport.expectedTime.toString("HH:mm") + " " + bloombergImport.timeZone.id),
    exportToLim     → bloombergImport.exportToLim
  ) }
}