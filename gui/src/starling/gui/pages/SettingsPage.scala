package starling.gui.pages

import java.awt.Dimension
import starling.gui._
import api.UserSettingUpdated
import starling.pivot.view.swing.MigPanel
import swing.event.ButtonClicked
import starling.gui.GuiUtils._
import StandardUserSettingKeys.{ExtraFormattingInfo, LiveDefault}
import javax.swing.{SpinnerNumberModel, JSpinner}
import swing._
import starling.daterange.{Day, Month}
import starling.pivot._
import starling.pivot.MonthFormat._
import starling.pivot.utils.PeriodPivotFormatter

case class SettingsPage() extends Page {
  def text = "Settings"
  def icon = StarlingIcons.im("/icons/16x16_settings.png")
  def build(reader:PageBuildingContext) = null
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension) = new SettingsPageComponent(context)
}

class SettingsPageComponent(context:PageContext) extends MigPanel("insets 0") with PageComponent {
  val currentSettings = context.getSetting(ExtraFormattingInfo, PivotFormatter.DefaultExtraFormatInfo)
  val dp = currentSettings.decimalPlaces
  val decimalPlacesPanel = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
    def createSpinner(initialValue:Int) = {
      val maxDP = 10
      val spinnerModel = new SpinnerNumberModel(initialValue, 0, maxDP, 1)
      new JSpinner(spinnerModel) {
        def format = {
          val num = getValue.asInstanceOf[Int]
          if (num > 0) {
            "#,##0." + List.fill(num)("0").mkString
          } else {
            "#,##0"
          }
        }
      }
    }

    def numFromText(t:String) = {
      val lastIndex = t.lastIndexOf(".")
      if (lastIndex == -1) {
        0
      } else {
        t.length - 1 - lastIndex
      }
    }

    val defaultSpinner = createSpinner(numFromText(dp.defaultFormat))
    val priceSpinner = createSpinner(numFromText(dp.priceFormat))
    val currencySpinner = createSpinner(numFromText(dp.currencyFormat))
    val lotsSpinner = createSpinner(numFromText(dp.lotsFormat))
    val percentSpinner = createSpinner(numFromText(dp.percentageFormat))

    add(LabelWithSeparator("Decimal Places"), "spanx, growx, wrap")
    add(new Label("Default:"), "skip 1")
    add(defaultSpinner, "wrap")
    add(new Label("Price:"), "skip 1")
    add(priceSpinner, "wrap")
    add(new Label("Currency:"), "skip 1")
    add(currencySpinner, "wrap")
    add(new Label("Lots:"), "skip 1")
    add(lotsSpinner, "wrap")
    add(new Label("Percent:"), "skip 1")
    add(percentSpinner)

    def decimalPlaces = DecimalPlaces(defaultSpinner.format, lotsSpinner.format, priceSpinner.format, currencySpinner.format, percentSpinner.format)
    def decimalPlaces_=(dp:DecimalPlaces) {
      defaultSpinner.setValue(numFromText(dp.defaultFormat))
      priceSpinner.setValue(numFromText(dp.priceFormat))
      currencySpinner.setValue(numFromText(dp.currencyFormat))
      lotsSpinner.setValue(numFromText(dp.lotsFormat))
      percentSpinner.setValue(numFromText(dp.percentageFormat))
    }
  }

  val currentLiveSetting = context.getSetting(LiveDefault, false)
  val generalPanel = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
    val defaultLiveCheckbox = new CheckBox("Default Live") {
      selected = currentLiveSetting
    }

    add(LabelWithSeparator("General"), "spanx, growx, wrap")
    add(defaultLiveCheckbox, "skip 1")
  }

  val dateRangeFormatPanel = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {

    val standardExtraInfo = ExtraFormatInfo(dateRangeFormat = DateRangeFormat(Standard))
    val shortExtraInfo = ExtraFormatInfo(dateRangeFormat = DateRangeFormat(Short))
    val reutersExtraInfo = ExtraFormatInfo(dateRangeFormat = DateRangeFormat(Reuters))

    val today = Day.today()
    val sampleMonths = List(today.asMonthObject, today.addMonths(1).asMonthObject)

    val standardSampleText = sampleMonths.map(m => PeriodPivotFormatter.format(m, standardExtraInfo).text).mkString("(", ", ", " ...)")
    val shortSampleText = sampleMonths.map(m => PeriodPivotFormatter.format(m, shortExtraInfo).text).mkString("(", ", ", " ...)")
    val reutersSampleText = sampleMonths.map(m => PeriodPivotFormatter.format(m, reutersExtraInfo).text).mkString("(", ", ", " ...)")

    val standardLabel = new Label(standardSampleText)
    val shortLabel = new Label(shortSampleText)
    val reutersLabel = new Label(reutersSampleText)

    val standardButton = new RadioButton("Standard")
    val shortButton = new RadioButton("Short")
    val reutersButton = new RadioButton("Reuters")
    val group = new ButtonGroup(standardButton, shortButton, reutersButton)

    val buttonToType = Map[AbstractButton,DateRangeFormat](
      standardButton -> DateRangeFormat(Standard),
      shortButton -> DateRangeFormat(Short),
      reutersButton -> DateRangeFormat(Reuters))
    val typeToButton = buttonToType.map{_.swap}

    add(LabelWithSeparator("Month Format"), "spanx, growx, wrap")
    add(standardButton, "skip1")
    add(standardLabel, "wrap")
    add(shortButton, "skip 1")
    add(shortLabel, "wrap")
    add(reutersButton, "skip 1")
    add(reutersLabel)

    def dateRangeFormat = {
      buttonToType(group.selected.get)
    }
    def dateRangeFormat_=(drf:DateRangeFormat) {
      group.select(typeToButton(drf))
    }

    dateRangeFormat = currentSettings.dateRangeFormat
  }

  val saveButton = new Button {
    text = "Save Settings"
    tooltip = "Save the specified settings and update affected pages"
    reactions += {
      case ButtonClicked(b) => {
        context.putSetting(ExtraFormattingInfo, ExtraFormatInfo(decimalPlacesPanel.decimalPlaces, dateRangeFormatPanel.dateRangeFormat))
        context.putSetting(LiveDefault, generalPanel.defaultLiveCheckbox.selected)
      }
    }
  }

  add(generalPanel, "gapright unrel, ay top")
  add(decimalPlacesPanel, "gapright unrel, ay top")
  add(dateRangeFormatPanel, "wrap unrel, ay top")
  add(saveButton, "spanx, ax right")

  reactions += {
    case UserSettingUpdated(ExtraFormattingInfo) => {
      val extraFormatInfo = context.getSetting(ExtraFormattingInfo)
      decimalPlacesPanel.decimalPlaces = extraFormatInfo.decimalPlaces
      dateRangeFormatPanel.dateRangeFormat = extraFormatInfo.dateRangeFormat
    }
    case UserSettingUpdated(LiveDefault) => {
      val b = context.getSetting(LiveDefault, false)
      generalPanel.defaultLiveCheckbox.selected = b
    }
  }
  listenTo(context.remotePublisher)
}