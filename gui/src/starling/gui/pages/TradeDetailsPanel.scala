package starling.gui.pages

import starling.gui.api.TradeSelection
import starling.pivot.{SomeSelection, AllSelection, Selection, Field}
import swing.{Alignment, Label}
import starling.browser.common.MigPanel

class TradeDetailsPanel(tradeSelection:TradeSelection)
        extends MigPanel("insets 0", "[p]50lp[p]50lp[p]") {
  private val boldFont = new Label().font.deriveFont(java.awt.Font.BOLD)

  val desk = tradeSelection.desk match {
    case None => "Not Used"
    case Some(ts) => ts.name
  }
  val excelTrades = tradeSelection.intradaySubgroup.map("Intraday trades from " + _).getOrElse("Not Used")

  def fieldsToLabels(predicate1:List[(Field,Selection)]) = {
    predicate1.map{case(field, selection) => {
      val nameLabel = new Label(field.name) {
        font = boldFont
        xAlignment = Alignment.Left
      }
      val valueText = selection match {
        case AllSelection => "All Data"
        case SomeSelection(values) => {
          if (values.size == 0) {
            "None"
          } else {
            values.mkString(", ")
          }
        }
      }
      (nameLabel, new Label(valueText))
    }}
  }

  val filterFieldValueLabels = fieldsToLabels(tradeSelection.tradePredicate.filter)
  val sb = new StringBuilder("<html>")
  for (cell <- tradeSelection.tradePredicate.selection) {
    for ((field, selection) <- cell) {
      sb.append("<b>")
      sb.append(field.name).append("</b> ")
      val valueText = selection match {
        case AllSelection => "All Selection"
        case SomeSelection(values) => {
          if (values.size == 0) {
            "None"
          } else {
            values.mkString(", ")
          }
        }
      }
      sb.append(valueText).append("<i> and</i> ")
    }
    val l = sb.length
    if (l > 12) {
      sb.delete(l - 12, l)
    }
    sb.append("<br><i>or</i> ")
  }
  val l = sb.length
  if (l > 14) {
    sb.delete(l - 14, l)
  }
  sb.append("</html>")
  val selectionText = sb.toString
  val selectionAvailable = selectionText.length > 13

  val staticPanel = new MigPanel("insets 0") {
    add(new Label("Desk") {xAlignment = Alignment.Left})
    add(new Label(desk) {font = boldFont}, "wrap")
    add(new Label("Excel Trades"){xAlignment = Alignment.Left})
    add(new Label(excelTrades){font = boldFont})    
  }

  val filterPanel = new MigPanel("insets 0", "[rel][p]") {
    add(new Label("Filters"){xAlignment = Alignment.Left}, "spanx, split, wrap")
    if (filterFieldValueLabels.isEmpty) {
      add(new Label("No filters selected"), "skip 1, wrap")
    } else {
      for ((fieldLabel, valueLabel) <- filterFieldValueLabels) {
        add(fieldLabel, "skip 1")
        add(valueLabel, "wrap")
      }
    }
  }

  val selectionPanel = new MigPanel("insets 0", "[rel][p]") {
    add(new Label("Selection"){xAlignment = Alignment.Left}, "spanx, split, wrap")
    if (selectionAvailable) {
      add(new Label(selectionText) {xAlignment = Alignment.Left}, "split, span, growx, skip 1")
    } else {
      add(new Label("No selection made"), "skip 1")
    }
  }

  add(staticPanel, "ay top")
  add(filterPanel, "ay top")
  add(selectionPanel, "ay top")
}