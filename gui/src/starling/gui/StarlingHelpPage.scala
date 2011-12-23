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
      "RowGrandTotals" -> StarlingIcons.RowGrandTotals,
      "ColumnTotals" -> StarlingIcons.ColumnTotals,
      "SubTotals" -> StarlingIcons.RowSubTotals,
      "Chart" -> StarlingIcons.Chart,
      "Copy" -> StarlingIcons.Copy,
      "Rotate" -> StarlingIcons.Rotate,
      "Lock" -> StarlingIcons.Lock,
      "Calculate" -> StarlingIcons.Calculate,

      "SaveReportConfiguration" -> StarlingIcons.icon(SaveReportConfiguration),
      "MarketData" -> StarlingIcons.icon(MarketData),
      "Refresh" -> StarlingIcons.icon(BrowserIcons.Refresh)
    )

    val links = Map("ReferenceData" -> ReferenceDataIndexPage) //Just an example
    HelpEntry(markup, icons, links)
  }
}