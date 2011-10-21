package starling.services

import starling.db.DB
import starling.pivot._
import scalaz.Scalaz._
import starling.daterange.TimeZone


class BloombergImportsReferenceData(eai: DB) extends UnfilteredPivotTableDataSource {
  val lim@List(limFolder, limColumn, limSymbol, limDescription, expectedTime) =
    fieldDetails("Folder", "Column", "Lim Symbol", "Description", "Expected Time")
  val bloomberg@List(quoteId, symbol) = fieldDetails("Quote Id", "Symbol")

  def fieldDetailsGroups = List(FieldDetailsGroup("Lim", lim), FieldDetailsGroup("Bloomberg", bloomberg))
  override val initialState = PivotFieldsState(rowFields = fields(quoteId), dataFields = fields(symbol :: lim))

  def unfilteredData(pfs: PivotFieldsState) = BloombergImport.importsFrom(eai).map { bloombergImport => fields(
    quoteId        → bloombergImport.quoteId,
    symbol         → (bloombergImport.symbol | ""),
    limSymbol      → (bloombergImport.limSymbol | ""),
    limFolder      → (bloombergImport.limFolder | ""),
    limColumn      → (bloombergImport.limColumn | ""),
    limDescription → (bloombergImport.limDescription | ""),
    expectedTime   → (bloombergImport.expectedTime(TimeZone.GMT).toString("HH:mm") + " GMT")
  ) }
}