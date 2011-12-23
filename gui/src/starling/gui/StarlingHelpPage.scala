package starling.gui

import io.Source
import pages.ReferenceDataIndexPage
import starling.browser.internal.BrowserIcons
import starling.browser.{PagePageFactory, HelpEntry}

object StarlingHelpPage {

  def starlingHelpEntry = {
    import StarlingIcons._
    val markup = Source.fromURL(StarlingHelpPage.getClass.getResource("/Help.txt")).getLines.mkString("\n")
    val icons = Map(
      "RowGrandTotals" -> RowGrandTotals,
      "ColumnTotals" -> ColumnTotals,
      "SubTotals" -> RowSubTotals,
      "Chart" -> Chart,
      "Copy" -> Copy,
      "Rotate" -> Rotate,
      "Lock" -> Lock,
      "Calculate" -> Calculate,
      "ExpandColumnsToFit" -> ExpandColumnsToFit,
      "RemoveZeros" -> RemoveZeros,
      "ClearPivot" -> ClearPivot,
      "DefaultLayout" -> DefaultLayout,
      "ColumnSubTotals" -> ColumnSubTotals,
      "Save" -> Save,
      "ResetEdits" -> ResetEdits,

      "SaveReportConfiguration" -> StarlingIcons.icon(SaveReportConfiguration),
      "MarketData" -> StarlingIcons.icon(MarketData),
      "Refresh" -> StarlingIcons.icon(BrowserIcons.Refresh)
    )

    val links = Map("ReferenceData" -> ReferenceDataIndexPage) //Just an example
    HelpEntry(markup, icons, links)
  }
}